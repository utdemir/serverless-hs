{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Deploy where

--------------------------------------------------------------------------------
import           Codec.Archive.Zip
import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BL
import           Data.FileEmbed
import           Data.List                  (find)
import           Data.List.Split            (splitOn)
import           Data.Monoid
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Yaml                  as Yaml
import           Network.AWS                as A
import           Network.AWS.CloudFormation as A
import           Network.AWS.S3 as S3
import           Network.AWS.Waiter         as A
import           Stratosphere               as S
import           System.Directory           (doesFileExist)
import           System.FilePath
import           System.Process
--------------------------------------------------------------------------------
import           Types
--------------------------------------------------------------------------------

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
    _       -> throwM $ InvalidFile
                          path
                          "Expected extensions: .json, .yaml or .yml"
  case config' of
    Left err  -> throwM $ InvalidFile path (T.pack err)
    Right cfg -> return cfg

--------------------------------------------------------------------------------

bucketIdOutputKey :: AWSRes 'AWSBucket 'AWSId
bucketIdOutputKey = AWSRes "bucketId"

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
    st:[] -> case find (== serverlessHsTag) $ st ^. sTags of
               Nothing ->
                 throwM $ AWSError "CloudFormation stack lookup failed."
                                   ("Given stack '"
                                    <> stackName
                                    <> "' is foreign to serverless-hs.")
               Just _  -> return . fmap AWSRes $ st ^. sStackId
    _     -> throwM $ AWSError "CloudFormation stack lookup failed."
                               "Unknown answer to DescribeStacks call."


awsCreateStack :: AWSRes 'AWSStack 'AWSName -> Template -> M (AWSRes 'AWSStack 'AWSId)
awsCreateStack (AWSRes stackName) tpl = do
  csrs <- send $ createStack stackName
    & csTemplateBody ?~ (T.decodeUtf8 . BL.toStrict $ encodeTemplate tpl)
    & csTags <>~ [serverlessHsTag]
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
                                       ("Accept: " <> T.pack (show err))
  return $ AWSRes stackId

awsDescribeStack :: AWSRes 'AWSStack 'AWSId -> M AWSStackResult
awsDescribeStack sid@(AWSRes stackId) = do
  stack' <- send $ describeStacks & dStackName ?~ stackId
  bucketId <- case stack' ^. dsrsStacks of
    [st] -> case find (( == Just (fromAWSRes bucketIdOutputKey)) . view oOutputKey)
                      (st ^. sOutputs)
                 >>= view oOutputValue of
      Just xs -> return $ AWSRes xs
      Nothing -> throwM $ AWSError "CloudFormation stack creation failed."
                                   "Created stack does not have a bucket."
    _ -> throwM $ AWSError "CloudFormation stack creation failed."
                           "Unknown answer from CloudFront." 
  return $ AWSStackResult sid bucketId
  

awsUpdateStack :: AWSRes 'AWSStack 'AWSId -> Template -> M ()
awsUpdateStack (AWSRes stackId) tpl = do
  usrs <- send $ updateStack stackId
    & usTemplateBody ?~ (T.decodeUtf8 . BL.toStrict $ encodeTemplate tpl)
  unless (usrs ^. usrsResponseStatus == 200) $
    throwM $ AWSError "CloudFormation stack update failed."
                      ("Status code: " <> T.pack (show $ usrs ^. usrsResponseStatus))

  await stackUpdateComplete (describeStacks & dStackName ?~ stackId) >>= \case
    AcceptSuccess -> return ()
    err           -> throwM $ AWSError "CloudFormation stack update timed out."
                                       ("Accept: " <> T.pack (show err))

awsUploadArchive :: AWSRes 'AWSBucket 'AWSId -> DeploymentZip -> M ()
awsUploadArchive (AWSRes bucketId) (DeploymentZip contents) = do
  pors <- send $ putObject
                   (BucketName bucketId)
                   (ObjectKey "deployment.zip")
                   (toBody contents)
  unless (pors ^. porsResponseStatus == 200) $
    throwM $ AWSError "Upload failed."
                      ("Status code: " <> T.pack (show $ pors ^. porsResponseStatus))  
    
  return ()
  
--------------------------------------------------------------------------------
