{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module Sandbox (runSandbox) where 

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Network.Mock.Chain (foldChain)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config.SecurityParam
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.Mock.Ledger.Block
import           Ouroboros.Consensus.Mock.Ledger.Block.PBFT
import           Ouroboros.Consensus.Mock.Node ()
import           Ouroboros.Consensus.Mock.Node.PBFT (MockPBftBlock,
                     protocolInfoMockPBFT)
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..))
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.PBFT
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util.Condense (condense)

import           Test.ThreadNet.General
import           Test.ThreadNet.Network
import qualified Test.ThreadNet.Ref.PBFT as Ref
import           Test.ThreadNet.TxGen.Mock ()
import           Test.ThreadNet.Util
import           Test.ThreadNet.Util.HasCreator.Mock ()
import           Test.ThreadNet.Util.NodeJoinPlan
import           Test.ThreadNet.Util.NodeRestarts
import           Test.ThreadNet.Util.NodeToNodeVersion
import           Test.ThreadNet.Util.SimpleBlock

import           Test.Util.HardFork.Future (singleEraFuture)
import           Test.Util.Orphans.Arbitrary ()

runSandbox :: IO ()
runSandbox = do
  putStrLn "sandbox"
  TestSetup{..} <- generate @TestSetup arbitrary
  let TestConfig{numCoreNodes, numSlots} = setupTestConfig
      slotLength = slotLengthFromSec 1
      params = PBftParams setupK numCoreNodes sigThd
      NumCoreNodes nn = numCoreNodes
      sigThd = PBftSignatureThreshold $ (1.0 / fromIntegral nn) + 0.1

      testConfigB = TestConfigB
        { forgeEbbEnv = Nothing
        , future      = singleEraFuture
            slotLength
            (EpochSize $ maxRollbacks setupK * 10)
            -- The mock ledger doesn't really care, and neither does PBFT. We
            -- stick with the common @k * 10@ size for now.
        , messageDelay = noCalcMessageDelay
        , nodeJoinPlan = setupNodeJoinPlan
        , nodeRestarts = noRestarts
        , txGenExtra   = ()
        , version      = newestVersion (Proxy @MockPBftBlock)
        }

      testOutput =
        runTestNetwork setupTestConfig testConfigB TestConfigMB
            { nodeInfo = plainTestNodeInitialization .
                            protocolInfoMockPBFT
                              params
                              (HardFork.defaultEraParams setupK slotLength)
            , mkRekeyM = Nothing
            }

  -- putStrLn $ show ts
  putStrLn $ "************************************"
  putStrLn $ show (testOutput `seq` "done")

  where
    
    


data TestSetup = TestSetup
  { setupK            :: SecurityParam
  , setupTestConfig   :: TestConfig
  , setupNodeJoinPlan :: NodeJoinPlan
  }
  deriving (Show)

instance Arbitrary TestSetup where
  arbitrary = do
      k <- SecurityParam <$> choose (1, 10)

      testConfig <- arbitrary
      let TestConfig{numCoreNodes, numSlots} = testConfig

      nodeJoinPlan <- genNodeJoinPlan numCoreNodes numSlots
      pure $ TestSetup k testConfig nodeJoinPlan