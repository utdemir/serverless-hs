{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedLists   #-}

module Main where

--------------------------------------------------------------------------------
import           Control.Exception.Safe
import Control.Lens
import           Control.Monad.IO.Class
import Data.Aeson
import           Data.Monoid            ((<>))
import           Data.Time.Clock
import           Options.Generic
import           System.Exit
import qualified Stratosphere as S
import qualified Data.ByteString.Lazy as BL
--------------------------------------------------------------------------------
import qualified Web.Serverless.Internal.Constants as C
import Web.Serverless.Internal.Handler
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

  step "Remote" $ do
    archiveName <- step "Uploading deployment archive" $ do
      now <- liftIO getCurrentTime
      let archiveName = mkArchiveName configStackName now archive
      awsUploadObject configBucketName archiveName (unDeploymentZip archive)
      return archiveName

    (sid, fid) <- step "Creating an initial stack" $ do
      sid <- awsCreateStack configStackName (configBucketName, archiveName) initialTemplate
      stack <- awsDescribeStack sid
      case "meta" `lookup` _awsStackResources stack of
        Just x -> return $ (sid, AWSRes x)
        Nothing -> error "foo"

    descr <- step "Generating deployment description" $ do
      obj <- awsInvokeFunction fid mempty
      case decodeValue $ Object obj of
        Left err -> error err
        Right desc -> return (desc :: S.Resources)
        

    step "Deploying" $ do
      let template = S.template descr
            & S.parameters ?~ [ S.parameter C.parameterS3Bucket "String"
                              , S.parameter C.parameterS3Key "String"
                              ]

      awsUpdateStack sid (configBucketName, archiveName) template
      
    liftIO $ print descr

    return ()

  return ()
