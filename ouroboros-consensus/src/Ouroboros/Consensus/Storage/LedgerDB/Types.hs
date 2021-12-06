{-# LANGUAGE DeriveGeneric #-}
module Ouroboros.Consensus.Storage.LedgerDB.Types (
    PushGoal (..)
  , Pushing (..)
  , UpdateLedgerDbTraceEvent (..)
  ) where


import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Block.RealPoint (RealPoint)

{-------------------------------------------------------------------------------
  Trace events
-------------------------------------------------------------------------------}
newtype PushGoal blk = PushGoal { unPushGoal :: RealPoint blk }
  deriving (Show, Eq)

newtype Pushing blk = Pushing { unPushing :: RealPoint blk }
  deriving (Show, Eq)

data UpdateLedgerDbTraceEvent blk =
    -- | Event fired when we are about to push a block to the LedgerDB
      StartedPushingBlockToTheLedgerDb
        !(Pushing blk)
        -- ^ Point which block we are about to push
        (PushGoal blk)
        -- ^ Point to which we are updating the ledger, the last event
        -- StartedPushingBlockToTheLedgerDb will have Pushing and PushGoal
        -- wrapping over the same RealPoint
  deriving (Show, Eq, Generic)
