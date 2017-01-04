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
    sid' <- step "Checking for existing stack" $ do
      r <- awsLookupStack configStackName
      case r of
        Just _  -> warn "Stack already exists, this will overwrite old stack."
        Nothing -> info "No stack found, will create a fresh one."
      return r

    sid <- case sid' of
      Just i  ->
        step "Updating stack" $
          awsUpdateStack i undefined $> i
      Nothing ->
        step "Creating an initial stack" $
          awsCreateStack configStackName undefined

    step "Uploading deployment archive" $ do
      bid <- awsStackBucketId sid
      awsUploadArchive bid archive

    step "Checking the executable" $ return ()

  return ()
