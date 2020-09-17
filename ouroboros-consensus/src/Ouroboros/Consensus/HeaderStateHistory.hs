{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}
-- | HeaderState history
--
-- Intended for qualified import
--
-- > import           Ouroboros.Consensus.HeaderStateHistory (HeaderStateHistory (..))
-- > import qualified Ouroboros.Consensus.HeaderStateHistory as HeaderStateHistory
module Ouroboros.Consensus.HeaderStateHistory (
    HeaderStateHistory (..)
  , current
  , trim
  , rewind
  , cast
    -- * Validation
  , validateHeader
    -- * Support for tests
  , fromChain
  ) where

import           Control.Exception (assert)
import           Control.Monad.Except (Except)
import           Data.Coerce (Coercible)
import           Data.Sequence.Strict (StrictSeq ((:|>), Empty))
import qualified Data.Sequence.Strict as Seq
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderValidation hiding (validateHeader)
import qualified Ouroboros.Consensus.HeaderValidation as HeaderValidation
import           Ouroboros.Consensus.Protocol.Abstract

-- Support for tests
import qualified Data.List.NonEmpty as NE
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Network.MockChain.Chain (Chain (..))
import qualified Ouroboros.Network.MockChain.Chain as Chain

-- | Maintain a history of 'HeaderState's.
data HeaderStateHistory blk = HeaderStateHistory {
      -- | The last, right-most, element in the sequence contains the current
      -- state, see 'headerStateHistoryCurrent'.
      headerStateHistorySnapshots :: !(StrictSeq (HeaderState blk))

      -- | Header state /before/ the oldest in 'headerStateHistorySnapshots'.
      -- This is the oldest tip we can roll back to.
    , headerStateHistoryAnchor    :: !(HeaderState blk)
    }
  deriving (Generic)

deriving instance (BlockSupportsProtocol blk, HasAnnTip blk)
                => Eq (HeaderStateHistory blk)
deriving instance (BlockSupportsProtocol blk, HasAnnTip blk)
                => Show (HeaderStateHistory blk)
deriving instance (BlockSupportsProtocol blk, HasAnnTip blk)
                => NoUnexpectedThunks (HeaderStateHistory blk)

current :: HeaderStateHistory blk -> HeaderState blk
current HeaderStateHistory {..} =
    case headerStateHistorySnapshots of
      _ :|> cur -> cur
      Empty     -> headerStateHistoryAnchor

currentPoint :: HasAnnTip blk => HeaderStateHistory blk -> Point blk
currentPoint = headerStatePoint . current

-- | Append a 'HeaderState' to the history.
append :: HeaderState blk -> HeaderStateHistory blk -> HeaderStateHistory blk
append headerState HeaderStateHistory {..} =
    HeaderStateHistory {
        headerStateHistorySnapshots = headerStateHistorySnapshots :|> headerState
      , headerStateHistoryAnchor    = headerStateHistoryAnchor
      }

-- | Trim the 'HeaderStateHistory' to the given size, dropping the oldest
-- snapshots. The anchor will be shifted accordingly.
--
-- Note that we do not include the anchor in the size. For example, trimming to
-- 0 results in no snapshots but still an anchor. Trimming to 1 results in 1
-- snapshot and an anchor.
trim :: Int -> HeaderStateHistory blk -> HeaderStateHistory blk
trim n history@HeaderStateHistory {..}
    | toDrop <= 0
    = history
    | otherwise
    = case Seq.splitAt toDrop headerStateHistorySnapshots of
        (_ :|> newAnchor, trimmed) -> HeaderStateHistory {
            headerStateHistorySnapshots = trimmed
          , headerStateHistoryAnchor    = newAnchor
          }
        (Empty, _) ->
          error $ "impossible: nothing dropped while toDrop = " <> show toDrop
  where
    toDrop :: Int
    toDrop = Seq.length headerStateHistorySnapshots - n

cast ::
     ( Coercible (ChainDepState (BlockProtocol blk ))
                 (ChainDepState (BlockProtocol blk'))
     , TipInfo blk ~ TipInfo blk'
     )
  => HeaderStateHistory blk -> HeaderStateHistory blk'
cast HeaderStateHistory {..} = HeaderStateHistory {
      headerStateHistorySnapshots = castHeaderState <$> headerStateHistorySnapshots
    , headerStateHistoryAnchor    = castHeaderState  $  headerStateHistoryAnchor
    }

-- | \( O\(n\) \). Rewind the header state history
--
-- NOTE: we don't distinguish headers of regular blocks from headers of EBBs.
-- Whenever we use \"header\" it can be either. In practice, EBB headers do not
-- affect the 'ChainDepState', but they /do/ affect the 'AnnTip'.
--
-- PRECONDITION: the point to rewind to must correspond to a header (or
-- 'GenesisPoint') that was previously applied to the header state history.
--
-- Rewinding the header state history is intended to be used when switching to a
-- fork, longer or equally long to the chain to which the current header state
-- corresponds. So each rewinding should be followed by rolling forward (using
-- 'headerStateHistoryPush') at least as many blocks that we have rewound.
--
-- Note that repeatedly rewinding a header state history does not make it
-- possible to rewind it all the way to genesis (this would mean that the whole
-- historical header state is accumulated or derivable from the current header
-- state history). For example, rewinding a header state by @i@ blocks and then
-- rewinding that header state again by @j@ where @i + j > k@ is not possible
-- and will yield 'Nothing'.
rewind ::
     forall blk. (BlockSupportsProtocol blk, HasAnnTip blk)
  => Point blk
  -> HeaderStateHistory blk -> Maybe (HeaderStateHistory blk)
rewind p HeaderStateHistory {..} =
    case Seq.dropWhileR rolledBack headerStateHistorySnapshots of
        Empty
          | rolledBack headerStateHistoryAnchor
            -- Asked to roll back past the anchor
          -> assert (pointSlot p <=
                     pointSlot (headerStatePoint headerStateHistoryAnchor))
             Nothing

        headerStateHistorySnapshots'
          | let history' = HeaderStateHistory {
                     headerStateHistorySnapshots = headerStateHistorySnapshots'
                   , headerStateHistoryAnchor
                   }
          -> assert (currentPoint history' == p) $
             Just history'
  where
    -- | Should the given 'HeaderState' be rolled back?
    rolledBack :: HeaderState blk -> Bool
    rolledBack headerState = headerStatePoint headerState /= p

{-------------------------------------------------------------------------------
  Validation
-------------------------------------------------------------------------------}

-- | Variation on 'HeaderValidation.validateHeader' that maintains a
-- 'HeaderStateHistory'.
--
-- This is used only in the chain sync client for header-only validation.
--
-- Note: this function does not trim the 'HeaderStateHistory'.
validateHeader ::
     forall blk. (BlockSupportsProtocol blk, ValidateEnvelope blk)
  => TopLevelConfig blk
  -> Ticked (LedgerView (BlockProtocol blk))
  -> Header blk
  -> HeaderStateHistory blk
  -> Except (HeaderError blk) (HeaderStateHistory blk)
validateHeader cfg ledgerView hdr history = do
    st' <- HeaderValidation.validateHeader cfg ledgerView hdr st
    return $ append st' history
  where
    st :: Ticked (HeaderState blk)
    st = tickHeaderState
           (configConsensus cfg)
           ledgerView
           (blockSlot hdr)
           (current history)

{-------------------------------------------------------------------------------
  Support for tests
-------------------------------------------------------------------------------}

-- | Create a 'HeaderStateHistory' corresponding to the blocks in the given
-- 'Chain'.
--
-- PRECONDITION: the blocks in the chain are valid.
fromChain ::
     ApplyBlock (ExtLedgerState blk) blk
  => TopLevelConfig blk
  -> ExtLedgerState blk
     -- ^ Initial ledger state
  -> Chain blk
  -> HeaderStateHistory blk
fromChain cfg initState chain = HeaderStateHistory {
      headerStateHistorySnapshots = Seq.fromList snapshots
    , headerStateHistoryAnchor    = anchor
    }
  where
    anchor NE.:| snapshots =
          fmap headerState
        . NE.scanl
            (flip (tickThenReapply (ExtLedgerCfg cfg)))
            initState
        . Chain.toOldestFirst
        $ chain