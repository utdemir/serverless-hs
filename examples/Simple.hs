module Main where

--------------------------------------------------------------------------------
import Data.Aeson
import Web.Serverless
--------------------------------------------------------------------------------

main :: IO ()
main = serverlessMain $ do
  function echo
  return ()

echo :: LambdaFunction Value String Value
echo = LambdaFunction $ \a ->
  return . Right . _eventPayload $  a
