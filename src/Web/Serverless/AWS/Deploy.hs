{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Web.Serverless.AWS.Deploy where

--------------------------------------------------------------------------------
import           Codec.Archive.Zip
import           Control.Exception.Safe
import           Control.Monad
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BL
import           Data.FileEmbed
import           Data.List.Split        (splitOn)
import           Stratosphere
import           System.Directory       (doesFileExist)
import           System.Environment     (getExecutablePath)
import           System.Process
import Network.AWS hiding (Ref)
import qualified Data.Text as T
import Network.AWS.Waiter
import Network.AWS.CloudFormation hiding (output)
import qualified Data.Text.Encoding as T
import Control.Monad.IO.Class
import Data.Monoid
import Data.Text (Text)
--------------------------------------------------------------------------------
import           Web.Serverless.Types
--------------------------------------------------------------------------------

newtype HandlerJS     = HandlerJS     BS.ByteString
newtype HsMain        = HsMain        BS.ByteString
newtype DeploymentZip = DeploymentZip BS.ByteString

handlerJS :: HandlerJS
handlerJS = HandlerJS $(embedFile "static/handler.js")

deploymentZip :: HandlerJS -> HsMain -> DeploymentZip
deploymentZip (HandlerJS handler) (HsMain executable)
  = DeploymentZip . BL.toStrict . fromArchive $ archive
  where
    archive
      = addEntryToArchive (toEntry "handler.js" 0 $ BL.fromStrict handler)
      $ addEntryToArchive (toEntry "hs-main" 0 $ BL.fromStrict executable)
      $ emptyArchive

currentExecutable :: IO FilePath
currentExecutable = getExecutablePath

mkHsMain :: FilePath -> IO HsMain
mkHsMain fp = check >> (HsMain <$> BS.readFile fp)
  where
    check = existence >> file
    existence = doesFileExist fp >>= \case
      False -> throw $ InvalidFile "File does not exist" ""
      True  -> return ()
    file = do
      ret <- splitOn ", " <$> readProcess "file" ["--brief", fp] ""

      let preds :: [(String, DeployException)]
          preds =  [ ( "ELF 64-bit LSB executable"
                     , InvalidFile "file is not an 64 bit ELF executable" ""
                     )
                   , ( "statically linked"
                     , InvalidFile "file is not statically linked" ""
                     )
                   ]

      forM_ preds $ \(str, ex) ->
        if str `elem` ret
          then return ()
          else throw ex

      return ()

mkDeploymentPackage :: DeployArgs -> IO DeploymentZip
mkDeploymentPackage DeployArgs { otherExecutable }
  = deploymentZip handlerJS
      <$> (maybe currentExecutable return otherExecutable >>= mkHsMain)

saveDeploymentPackage :: FilePath -> DeploymentZip -> IO ()
saveDeploymentPackage fp (DeploymentZip contents) = BS.writeFile fp contents

--------------------------------------------------------------------------------

bucketResource :: Resource
bucketResource = resource "bucket" $ S3BucketProperties s3Bucket

initialTemplate :: Template
initialTemplate
  = template
     [ bucketResource
     ]
--     & outputs ?~ [ output "bucket" (Ref $ bucketResource ^. resName)
--                  ]

stackName :: Text
stackName = "serverless-hs"

awsDeploy :: DeployArgs -> IO ()
awsDeploy args = do
  env  <- newEnv Discover
  ret <- runResourceT $ runAWS env $ do
    csrs <- send $ createStack stackName
      & csTemplateBody ?~ (T.decodeUtf8 . BL.toStrict $ encodeTemplate initialTemplate)
    unless (csrs ^. csrsResponseStatus == 200) $
      liftIO . throw $ AWSError "Initial template creation failed."
                                ("Status code: " <> T.pack (show $ csrs ^. csrsResponseStatus))

    await stackCreateComplete (describeStacks & dStackName ?~ stackName)
      >>= \case AcceptSuccess -> return ()
                err           -> liftIO . throw $ AWSError "Initial template creation failed."
                                                  ("Accept: " <> T.pack (show err))
                                      
    stackId <- case csrs ^. csrsStackId of
      Nothing -> liftIO . throw $ AWSError "Initial template creation failed."
                                           "Cannot determine stack id."
      Just t  -> return t
    buc <- send $ describeStackResource stackId (bucketResource ^. resName) 
    return buc

  print ret

  return ()


