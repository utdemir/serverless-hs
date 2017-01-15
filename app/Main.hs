{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

--------------------------------------------------------------------------------
import Data.Monoid ((<>))
import           Control.Exception.Safe
import Control.Monad.IO.Class
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
    archive <- step "Creating deployment package" $ do
      let a = deploymentZip handlerJS hsMain
      info $! "Deployment archive size: " <> deploymentZipSize a
      return a
    return (config, archive)

  saveDeploymentZip "test/deployment.zip" archive
  _ <- liftIO exitSuccess

  step "Remote" $ do
    step "Uploading deployment archive" $
      awsUploadArchive configBucketName archive

    stack <- step "Creating an initial stack" $
      awsCreateStack configStackName (initialTemplate configBucketName)
      >>= awsDescribeStack

    step "Checking the executable" $ return ()

  return ()
