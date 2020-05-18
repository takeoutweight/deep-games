module DebugGameTrain where

bob :: Int
bob = 3

{-
trainIt :: IO ()
trainIt = do
  deviceStr <- try (getEnv "DEVICE") :: IO (Either SomeException String)
  case deviceStr of
    Right "cpu"    -> train' @'( 'D.CPU, 0)
    Right "cuda:0" -> train' @'( 'D.CUDA, 0)
    _              -> error "Don't know what to do or how."
-}
