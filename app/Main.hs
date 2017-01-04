{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

--------------------------------------------------------------------------------
import           Control.Exception.Safe
import           Data.Functor           (($>))
import           Options.Generic
import           System.Exit
--------------------------------------------------------------------------------
import           Deploy
import           Types
--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getRecord "Serverless Haskell"

  try (deploy args) >>= \case
    Left ex  -> putStrLn (displayException (ex :: SomeException)) >> exitFailure
    Right () -> return ()

deploy :: Args -> IO ()
deploy Args{..} = runM $ do
  (Config{..}, archive) <- step "Local" $ do
    (config, hsMain) <- step "Reading configuration." $ do
      config <- step "Checking configuration file" $
        readConfig configPath
      hsMain <- step "Reading executable" $
        mkHsMain executablePath
      return (config, hsMain)
    archive <- step "Creating deployment package" $
      return $ deploymentZip handlerJS hsMain
    return (config, archive)

  step "Remote" $ do
    stack <- step "Creating an initial stack" $
      awsCreateStack configStackName undefined
      >>= awsDescribeStack

    step "Uploading deployment archive" $
      awsUploadArchive (_awsStackBucket stack) archive

    step "Checking the executable" $ return ()

  return ()
