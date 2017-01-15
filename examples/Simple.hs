module Main where

--------------------------------------------------------------------------------
import Data.Aeson
import Web.Serverless
--------------------------------------------------------------------------------

main :: IO ()
main = serverlessMain echo

echo :: LambdaFunction Value String IO Value
echo = LambdaFunction $ \a ->
  return . Right . _eventPayload $  a
