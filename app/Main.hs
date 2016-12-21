module Main where

--------------------------------------------------------------------------------
import System.Environment
import Lib
import Types
import Data.Functor
--------------------------------------------------------------------------------

main :: IO ()
main = do
  source:sink:[] <- getArgs
  fileServer source sink myFun $> ()

myFun :: LambdaFunction Int () IO Int
myFun = LambdaFunction $ \event -> do
  return . Right $ _eventPayload event * 2
