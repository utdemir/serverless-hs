{-# LANGUAGE BinaryLiterals    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Deploy where

--------------------------------------------------------------------------------
import           Codec.Archive.Zip
import Data.Maybe
import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import qualified Data.Aeson                   as Aeson
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as BL
import           Data.Digest.Pure.SHA
import           Data.FileEmbed
import           Data.List                    (find)
import           Data.List.Split              (splitOn)
import           Data.Monoid
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import           Data.Time.Clock
import           Data.Time.Format
import qualified Data.Yaml                    as Yaml
import           Network.AWS                  as A
import           Network.AWS.CloudFormation   as A
import qualified Data.HashMap.Strict as HM
import           Network.AWS.Lambda.Invoke   as A
import           Network.AWS.S3               as S3
import           Network.AWS.Waiter           as A
import           Numeric
import qualified Stratosphere                 as S
import           System.Directory             (doesFileExist)
import           System.FilePath
import           System.Process
--------------------------------------------------------------------------------
import           Types
import           Web.Serverless.Internal.Spec
import           qualified Web.Serverless.Internal.Constants as C
--------------------------------------------------------------------------------

handlerJS :: HandlerJS
handlerJS = HandlerJS $(embedFile "static/handler.js")

deploymentZip :: HandlerJS -> HsMain -> DeploymentZip
deploymentZip (HandlerJS handler) (HsMain executable)
  = DeploymentZip . BL.toStrict . fromArchive $ archive
  where
    archive
      = addEntryToArchive (toEntry "index.js" 0 $ BL.fromStrict handler)
      $ addEntryToArchive (toEntry "hs-main" 0 $ BL.fromStrict executable) {
          eExternalFileAttributes = 0b10000 -- rwx, found by trial&error
        }
      $ emptyArchive

deploymentZipSize :: DeploymentZip -> T.Text
deploymentZipSize (DeploymentZip contents)
  = let size = BS.length contents
        mbs  = fromIntegral size / (1024*1024) :: Double
    in  T.pack $ showFFloat (Just 2) mbs " MB"

saveDeploymentZip :: FilePath -> DeploymentZip -> M ()
saveDeploymentZip fp (DeploymentZip contents)
  = liftIO $ BS.writeFile fp contents

mkHsMain :: FilePath -> M HsMain
mkHsMain fp = check >> (HsMain <$> liftIO (BS.readFile fp))
  where
    check = existence >> file
    existence = liftIO (doesFileExist fp) >>= \case
      False -> throwM $ InvalidFile "File does not exist" (T.pack fp)
      True  -> return ()
    file = do
      ret <- liftIO $ splitOn ", " <$> readProcess "file" ["--brief", fp] ""

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
          else throwM ex

      return ()

--------------------------------------------------------------------------------

readConfig :: FilePath -> M Config
readConfig path = do
  let rf = BS.readFile path
  config' <- liftIO $ case takeExtension path of
    ".json" -> Aeson.eitherDecodeStrict <$> rf
    ".yml"  -> Yaml.decodeEither        <$> rf
    ".yaml" -> Yaml.decodeEither        <$> rf
    _       -> return $ Left "Expected extensions: .json, .yaml or .yml"
  case config' of
    Left err  -> throwM $ InvalidFile path (T.pack err)
    Right cfg -> return cfg

--------------------------------------------------------------------------------

bucketIdOutputKey :: T.Text
bucketIdOutputKey = "bucketId"

serverlessHsTag :: A.Tag
serverlessHsTag = A.tag & (A.tagKey   ?~ "serverless-hs")
                        & (A.tagValue ?~ "serverless-hs")

awsLookupStack :: AWSRes 'AWSStack 'AWSName -> M (Maybe (AWSRes 'AWSStack 'AWSId))
awsLookupStack (AWSRes stackName) = do
  dsrs <- send $ describeStacks & dStackName ?~ stackName
  unless (dsrs ^. dsrsResponseStatus == 200) $
    throwM $ AWSError "CloudFormation stack lookup failed."
                      ("Status code: " <> T.pack (show $ dsrs ^. dsrsResponseStatus))
  case dsrs ^. dsrsStacks of
    []    -> return Nothing
    st:[] -> case find (== serverlessHsTag) $ st ^. A.sTags of
               Nothing ->
                 throwM $ AWSError "CloudFormation stack lookup failed."
                                   ("Given stack '"
                                    <> stackName
                                    <> "' is foreign to serverless-hs.")
               Just _  -> return . fmap AWSRes $ st ^. sStackId
    _     -> throwM $ AWSError "CloudFormation stack lookup failed."
                               "Unknown answer to DescribeStacks call."

awsCreateStack :: AWSRes 'AWSStack 'AWSName
               -> (AWSRes 'AWSBucket ty, T.Text)
               -> S.Template
               -> M (AWSRes 'AWSStack 'AWSId)
awsCreateStack (AWSRes stackName) (AWSRes bucketName, path) tpl = do
  csrs <- send $ createStack stackName
    & csTemplateBody ?~ (T.decodeUtf8 . BL.toStrict $ S.encodeTemplate tpl)
    & csTags <>~ [serverlessHsTag]
    & csCapabilities <>~ [CapabilityIAM]
    & csParameters <>~ [ parameter
                           & pParameterKey ?~ C.parameterS3Bucket
                           & pParameterValue ?~ bucketName
                       , parameter
                           & pParameterKey ?~ C.parameterS3Key
                           & pParameterValue ?~ path
                       ]
  unless (csrs ^. csrsResponseStatus == 200) $
    throwM $ AWSError "CloudFormation stack creation failed."
                      ("Status code: " <> T.pack (show $ csrs ^. csrsResponseStatus))

  stackId <- case csrs ^. csrsStackId of
    Nothing -> throwM $ AWSError "CloudFormation stack creation failed."
                                 "Could not determine stack id."
    Just xs -> return xs

  await stackCreateComplete (describeStacks & dStackName ?~ stackId) >>= \case
    AcceptSuccess -> return ()
    err           -> throwM $ AWSError "CloudFormation stack creation timed out."
                                       (T.pack $ show err)
  return $ AWSRes stackId

awsUpdateStack :: AWSRes 'AWSStack 'AWSId
               -> (AWSRes 'AWSBucket ty, T.Text)
               -> S.Template
               -> M ()
awsUpdateStack (AWSRes stackId) (AWSRes bucketName, path) tpl = do
  usrs <- send $ updateStack stackId
    & usTemplateBody ?~ (T.decodeUtf8 . BL.toStrict $ S.encodeTemplate tpl)
    & usParameters <>~ [ parameter
                           & pParameterKey ?~ C.parameterS3Bucket
                           & pParameterValue ?~ bucketName
                       , parameter
                           & pParameterKey ?~ C.parameterS3Key
                           & pParameterValue ?~ path
                       ]
    & usCapabilities <>~ [CapabilityIAM]
  unless (usrs ^. usrsResponseStatus == 200) $
    throwM $ AWSError "CloudFormation stack update failed."
                      ("Status code: " <> T.pack (show $ usrs ^. usrsResponseStatus))

  await stackUpdateComplete (describeStacks & dStackName ?~ stackId) >>= \case
    AcceptSuccess -> return ()
    err           -> throwM $ AWSError "CloudFormation stack update timed out."
                                       ("Accept: " <> T.pack (show err))

awsDescribeStack :: AWSRes 'AWSStack 'AWSId -> M AWSStackResult
awsDescribeStack (AWSRes stackId) = do
  dsrsrs <- send $ describeStackResources & dsrStackName ?~ stackId
  unless (dsrsrs ^. dsrsrsResponseStatus == 200) $
    throwM $ AWSError "CloudFormation describeStack failed."
                      ("Status code: " <> T.pack (show $ dsrsrs ^. dsrsrsResponseStatus))
  let resources = mapMaybe
        (\s -> (,) (s^.srLogicalResourceId) <$> s^.srPhysicalResourceId)
        (dsrsrs ^. dsrsrsStackResources)

  return $ AWSStackResult (AWSRes stackId) resources

awsUploadObject :: AWSRes 'AWSBucket ty -> T.Text -> BS.ByteString -> M ()
awsUploadObject (AWSRes bucketId) path contents = do
  pors <- send $ putObject
                   (BucketName bucketId)
                   (ObjectKey path)
                   (toBody contents)
  unless (pors ^. porsResponseStatus == 200) $
    throwM $ AWSError "Upload failed."
                      ("Status code: " <> T.pack (show $ pors ^. porsResponseStatus))

  return ()

awsInvokeFunction :: AWSRes 'AWSLambdaFunction 'AWSId -> Aeson.Object -> M Aeson.Object
awsInvokeFunction (AWSRes functionId) payload = do
  irs <- send $ invoke functionId payload
  unless (irs ^. irsStatusCode == 200) $
    throwM $ AWSError "Failed to invoke lambda function."
                      ("Status code: " <> T.pack (show $ irs ^. irsStatusCode))

  
  case irs ^. irsPayload of
    Nothing ->  throwM $ AWSError "Failed to get answer from lambda function."
                                 ("Status code: " <> T.pack (show $ irs ^. irsStatusCode))
    Just x -> return x

  

mkArchiveName :: AWSRes 'AWSStack 'AWSName -> UTCTime -> DeploymentZip -> T.Text
mkArchiveName (AWSRes stackName) date' (DeploymentZip contents)
  = let hash = sha1 $ BL.fromStrict contents
        date = formatTime defaultTimeLocale (iso8601DateFormat Nothing) date'
    in T.intercalate "-"
         ["serverless-hs", stackName, T.pack date, T.pack $ showDigest hash]
         <> ".zip"

--------------------------------------------------------------------------------

initialTemplate :: S.Template
initialTemplate = S.template
  [ S.resource "role" defaultRole
  , S.resource "meta" $ S.LambdaFunctionProperties
      $ S.lambdaFunction
      ( S.lambdaFunctionCode
        & S.lfcS3Bucket ?~ S.Ref C.parameterS3Bucket
        & S.lfcS3Key    ?~ S.Ref C.parameterS3Key
      )
      "index.handler"
      (S.GetAtt "role" "Arn")
      (S.Literal S.NodeJS43)
      & S.lfTimeout ?~ S.Literal 10
      & S.lfEnvironment ?~ S.LambdaFunctionEnvironment
          (Just $ HM.fromList [(C.envFunctionId, "meta")])
  ]
  & S.parameters ?~ [ S.parameter C.parameterS3Bucket "String"
                    , S.parameter C.parameterS3Key "String"
                    ]

