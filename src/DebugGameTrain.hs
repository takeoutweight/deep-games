{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ExplicitNamespaces #-}

module DebugGameTrain where

import Control.Monad as CM
import Torch.Typed.Functional
import Control.Exception.Safe (SomeException(..), try)
import GHC.TypeLits (KnownNat ,Nat)
import qualified           GHC.Generics as G
import qualified GHC.TypeLits as TL
import qualified System.Environment as SE
import qualified Torch.HList as THL
import qualified Torch.Functional as TF
import qualified Torch.Tensor                  as TTensor
import qualified Torch.Typed.Autograd as TTA
import qualified Torch.Typed.Parameter as TTP
import qualified Torch.Typed.Aux as Aux
import qualified Torch.Typed.Tensor as TT
import qualified Torch.Typed.Functional as TTF
import qualified Torch.Typed.Factories as TFac
import qualified Torch.Typed.NN as TNN
import qualified Torch.Typed.Optim as TTO
import qualified Torch.Typed.Parameter as TTP
import qualified Torch.Typed.Serialize               as TSer
import qualified Torch.Internal.Class                    as TT.Class
import qualified Torch.Internal.Managed.Type.Context as TT.Context

import qualified Torch.Device as Torch.Device
import qualified Torch.DType as DType
import qualified Torch.NN                      as NN

type Rows = 28
type Cols = 28
type DataDim = Rows TL.* Cols
type ClassDim = 10
type BatchSize = 512
type HiddenFeatures0 = 512
type HiddenFeatures1 = 256

data GameData = GameData

gdLength :: GameData -> Int
gdLength gd = 8

initGameData :: IO (GameData, GameData)
initGameData = undefined

data MLPSpec (inputFeatures :: Nat) (outputFeatures :: Nat)
             (hiddenFeatures0 :: Nat) (hiddenFeatures1 :: Nat)
             (dtype :: DType.DType)
             (device :: (Torch.Device.DeviceType, Nat))
 where
  MLPSpec
    :: forall inputFeatures outputFeatures hiddenFeatures0 hiddenFeatures1 dtype device
     . { mlpDropoutProbSpec :: Double }
    -> MLPSpec inputFeatures outputFeatures hiddenFeatures0 hiddenFeatures1 dtype device
 deriving (Show, Eq)

data MLP (inputFeatures :: Nat) (outputFeatures :: Nat)
         (hiddenFeatures0 :: Nat) (hiddenFeatures1 :: Nat)
         (dtype :: DType.DType)
         (device :: (Torch.Device.DeviceType, Nat))
 where
  MLP
    :: forall inputFeatures outputFeatures hiddenFeatures0 hiddenFeatures1 dtype device
     . { mlpLayer0  :: TNN.Linear inputFeatures   hiddenFeatures0 dtype device
       , mlpLayer1  :: TNN.Linear hiddenFeatures0 hiddenFeatures1 dtype device
       , mlpLayer2  :: TNN.Linear hiddenFeatures1 outputFeatures  dtype device
       , mlpDropout :: TNN.Dropout
       }
    -> MLP inputFeatures outputFeatures hiddenFeatures0 hiddenFeatures1 dtype device
 deriving (Show, G.Generic)

mlp
  :: forall
       batchSize
       inputFeatures outputFeatures
       hiddenFeatures0 hiddenFeatures1
       dtype
       device
   . (Aux.StandardFloatingPointDTypeValidation device dtype)
  => MLP inputFeatures outputFeatures
         hiddenFeatures0 hiddenFeatures1
         dtype
         device
  -> Bool
  -> TT.Tensor device dtype '[batchSize, inputFeatures]
  -> IO (TT.Tensor device dtype '[batchSize, outputFeatures])
mlp MLP {..} train input =
  return
    .   TNN.linear mlpLayer2
    =<< TNN.dropout mlpDropout train
    .   TTF.tanh
    .   TNN.linear mlpLayer1
    =<< TNN.dropout mlpDropout train
    .   TTF.tanh
    .   TNN.linear mlpLayer0
    =<< pure input

instance ( KnownNat inputFeatures
         , KnownNat outputFeatures
         , KnownNat hiddenFeatures0
         , KnownNat hiddenFeatures1
         , TT.KnownDType dtype
         , TT.KnownDevice device
         , TFac.RandDTypeIsValid device dtype
         )
  => NN.Randomizable (MLPSpec inputFeatures outputFeatures hiddenFeatures0 hiddenFeatures1 dtype device)
                    (MLP     inputFeatures outputFeatures hiddenFeatures0 hiddenFeatures1 dtype device)
 where
  sample MLPSpec {..} =
    MLP
      <$> NN.sample TNN.LinearSpec
      <*> NN.sample TNN.LinearSpec
      <*> NN.sample TNN.LinearSpec
      <*> NN.sample (TNN.DropoutSpec mlpDropoutProbSpec)

foldLoop
  :: forall a b m . (Num a, Enum a, Monad m) => b -> a -> (b -> a -> m b) -> m b
foldLoop x count block = foldM block x ([1 .. count] :: [a])

foldLoop_
  :: forall a b m . (Num a, Enum a, Monad m) => b -> a -> (b -> a -> m b) -> m ()
foldLoop_ = ((CM.void .) .) . foldLoop

crossEntropyLoss
  :: forall batchSize seqLen dtype device
   . ( KnownNat batchSize
     , KnownNat seqLen
     , TT.KnownDType dtype
     , TT.KnownDevice device
     , Aux.StandardFloatingPointDTypeValidation device dtype
     )
  => TT.Tensor device dtype '[batchSize, seqLen]
  -> TT.Tensor device 'DType.Int64 '[batchSize]
  -> TT.Tensor device dtype '[]
crossEntropyLoss prediction target =
  TTF.nllLoss @TF.ReduceMean @batchSize @seqLen @'[]
    TFac.ones
    (-100)
    (TTF.logSoftmax @1 prediction)
    target

errorCount
  :: forall batchSize outputFeatures device
   . ( KnownNat batchSize
     , KnownNat outputFeatures
     , TTF.SumDTypeIsValid device 'DType.Bool
     , TT.ComparisonDTypeIsValid device 'DType.Int64
     )
  => TT.Tensor device 'DType.Float '[batchSize, outputFeatures] -- ^ prediction
  -> TT.Tensor device 'DType.Int64 '[batchSize] -- ^ target
  -> TT.Tensor device 'DType.Float '[]
errorCount prediction = TT.toDType @DType.Float . TTF.sumAll . TT.ne (TTF.argmax @1 @TTF.DropDim prediction)

train ::
     forall (batchSize :: Nat) (device :: (Torch.Device.DeviceType, Nat)) model optim gradients parameters tensors.
     ( KnownNat batchSize
     , Aux.StandardFloatingPointDTypeValidation device 'DType.Float
     , TTF.SumDTypeIsValid device 'DType.Bool
     , TT.ComparisonDTypeIsValid device 'DType.Int64
     , TT.KnownDevice device
     , TTA.HasGrad (THL.HList parameters) (THL.HList gradients)
     , tensors ~ gradients
     , THL.HMap' TTP.ToDependent parameters tensors
     , TT.Class.Castable (THL.HList gradients) [TTensor.ATenTensor]
     , TTP.Parameterized model parameters
     , TTO.Optimizer optim gradients tensors 'DType.Float device
     , THL.HMapM' IO TTP.MakeIndependent tensors parameters
     )
  => model
  -> optim
  -> (model -> Bool -> TT.Tensor device 'DType.Float '[ batchSize, DataDim] -> IO (TT.Tensor device 'DType.Float '[ batchSize, ClassDim]))
  -> TTO.LearningRate device 'DType.Float
  -> String
  -> IO ()
train initModel initOptim forward learningRate ptFile = do
  let numEpochs = 1000
  (trainingData, testData) <- initGameData
  foldLoop_ (initModel, initOptim) numEpochs $ \(epochModel, epochOptim) epoch -> do
    let numIters = gdLength trainingData `div` Aux.natValI @batchSize
    (epochModel', epochOptim') <- foldLoop (epochModel, epochOptim) numIters $ \(model, optim) i -> do
      (trainingLoss,_) <- computeLossAndErrorCount @batchSize (forward model True) 
                                                              i
                                                              trainingData
      (model', optim') <- TTO.runStep model optim trainingLoss learningRate
      return (model', optim')

    (testLoss, testError) <- do
      let numIters = gdLength testData `div` Aux.natValI @batchSize
      foldLoop (0,0) numIters $ \(org_loss,org_err) i -> do
        (loss,err) <- computeLossAndErrorCount @batchSize (forward epochModel' False)
                                                          i
                                                          testData
        return (org_loss + TT.toFloat loss,org_err + TT.toFloat err)
    putStrLn
      $  "Epoch: "
      <> show epoch
      <> ". Test loss: "
      <> show (testLoss / realToFrac (gdLength testData))
      <> ". Test error-rate: "
      <> show (testError / realToFrac (gdLength testData))
    
    TSer.save (THL.hmap' TTP.ToDependent . TTP.flattenParameters $ epochModel') ptFile
    return (epochModel', epochOptim')
    
 where
  computeLossAndErrorCount
    :: forall n (device :: (Torch.Device.DeviceType, Nat))
    . _
    => (TT.Tensor device 'DType.Float '[n, DataDim] -> IO (TT.Tensor device 'DType.Float '[n, ClassDim]))
    -> Int
    -> GameData
    -> IO
         ( TT.Tensor device 'DType.Float '[]
         , TT.Tensor device 'DType.Float '[]
         )
  computeLossAndErrorCount forward' index_of_batch data' = do
    let from = (index_of_batch-1) * Aux.natValI @n
        to = (index_of_batch * Aux.natValI @n) - 1
        indexes = [from .. to]
        input  = TT.toDevice @device $ undefined -- I.getImages @n data' indexes
        target = TT.toDevice @device $ undefined -- I.getLabels @n data' indexes
    prediction <- forward' input
    return (crossEntropyLoss prediction target, errorCount prediction target)

train'
  :: forall (device :: (Torch.Device.DeviceType, Nat))
   . _
  => IO ()
train' = do
  let dropoutProb  = 0.5
      learningRate = 0.1
  TT.Context.manual_seed_L 123
  initModel <- NN.sample
    (MLPSpec @DataDim @ClassDim
             @HiddenFeatures0 @HiddenFeatures1
             @DType.Float
             @device
             dropoutProb
    )
  let initOptim = TTO.mkAdam 0 0.9 0.999 (TTP.flattenParameters initModel)
  train @BatchSize @device initModel initOptim mlp learningRate "static-mnist-mlp.pt"

trainIt :: IO ()
trainIt = do
  deviceStr <- try (SE.getEnv "DEVICE") :: IO (Either SomeException String)
  case deviceStr of
    Right "cpu"    -> train' @'( 'Torch.Device.CPU, 0)
    Right "cuda:0" -> train' @'( 'Torch.Device.CUDA, 0)
    _              -> error "Don't know what to do or how."
