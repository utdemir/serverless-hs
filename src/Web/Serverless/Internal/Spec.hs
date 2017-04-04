{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs             #-}

module Web.Serverless.Internal.Spec
  ( handleRef
  , Handle (handleToText)
  , SpecM
  , function, resource
  , describeSpecM
  , executeSpecM
  , metaSpec
  , defaultRole
  ) where

--------------------------------------------------------------------------------
import Data.List (inits)
import Control.Lens
import Control.Arrow
import Control.Monad.Writer
import qualified Data.Text as T
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Operational
import qualified Data.Map as M
import Stratosphere (ResourceProperties)
import qualified Stratosphere as S
import Data.Aeson
import qualified Data.HashMap.Strict as HM
--------------------------------------------------------------------------------
import Web.Serverless.Internal.Types
import Web.Serverless.Internal.Constants
--------------------------------------------------------------------------------

data Handle res
  = Handle
      { handleLocalName :: T.Text
      , handleToText :: T.Text
      }

handleRef :: Handle S.ResourceProperties -> S.Val T.Text
handleRef = S.Ref . handleToText

castHandle :: Handle a -> Handle b
castHandle (Handle a b) = Handle a b

data Spec res where
  SpecFunction :: LambdaFunction payload err ret
               -> Spec (Handle (LambdaFunction payload err ret))
  SpecResource :: S.ResourceProperties
               -> Spec (Handle ResourceProperties)

type SpecM a = Program Spec a

function :: LambdaFunction payload err ret
         -> SpecM (Handle (LambdaFunction payload err ret))
function = singleton . SpecFunction

resource :: ResourceProperties -> SpecM (Handle (ResourceProperties))
resource = singleton . SpecResource

--------------------------------------------------------------------------------

getResourceName :: MonadState Int m => m T.Text
getResourceName = (\i -> resourceName <> T.pack i) . show <$> state (id &&& succ)

describeSpecM :: SpecM () -> S.Resources
describeSpecM spec =
  let resources :: [S.Resource] = flip evalState 0 . execWriterT $ interpretWithMonad eval spec
  in  S.Resources $ updateEnvs resources
  where
    eval :: Spec a -> WriterT [S.Resource] (State Int) a
    eval (SpecResource res) = do
      name <- getResourceName
      tell [S.resource name res]
      return $ Handle name name
    eval (SpecFunction _) = 
      castHandle <$> interpretWithMonad eval functionResources

    updateEnvs :: [S.Resource] -> [S.Resource]
    updateEnvs resources =
      let names = map (S.view S.resName) resources
      in map (uncurry $ over S.properties . updateEnv) (zip (inits names) resources)

    updateEnv :: [T.Text] -> S.ResourceProperties -> S.ResourceProperties
    updateEnv names (S.LambdaFunctionProperties props)
      = S.LambdaFunctionProperties $
          props & S.lfEnvironment ?~ S.LambdaFunctionEnvironment (Just vars)
      where vars = HM.fromList [ (n, toJSON @(S.Val T.Text) $ S.Ref n)
                               | n <- names
                               ]
    updateEnv _ props = props

--------------------------------------------------------------------------------

executeSpecM :: forall val
              . (forall payload err ret. LambdaFunction payload err ret -> val)
             -> [(T.Text, T.Text)]
             -> SpecM ()
             -> Either String [(T.Text, val)]
executeSpecM fun env_ spec
  = flip evalState 0 $ runExceptT $ execWriterT $ interpretWithMonad eval spec
  where
    env = M.fromList env_
    
    eval :: Spec a -> WriterT [(T.Text, val)] (ExceptT String (State Int)) a
    eval (SpecResource _) = do
      envName <- getResourceName
      case M.lookup envName env of
        Nothing -> throwError $ "Required env not found: " ++ T.unpack envName
        Just val -> return $ Handle envName val
    eval (SpecFunction f) = do
      r <- interpretWithMonad eval functionResources
      tell [(handleLocalName r, fun f)]
      return $ castHandle r

--------------------------------------------------------------------------------

functionResources :: SpecM (Handle ())
functionResources = do
  role <- resource defaultRole
  fun <- resource $ S.LambdaFunctionProperties
         $ S.lambdaFunction
             ( S.lambdaFunctionCode
               & S.lfcS3Bucket ?~ S.Ref parameterS3Bucket
               & S.lfcS3Key    ?~ S.Ref parameterS3Key
             )
             "index.handler"
             (S.GetAtt (handleToText role) "Arn") -- (S.GetAtt (handleToText role) "Arn")
             (S.Literal S.NodeJS43)
             & S.lfTimeout ?~ S.Literal 10
  return $ castHandle fun

defaultRole :: S.ResourceProperties
defaultRole = S.IAMRoleProperties $ S.iamRole
  [ ( "Version"
    , "2012-10-17"
    )
  , ( "Statement"
    , object [ ( "Effect", "Allow")
             , ( "Principal"
               , object [ ( "Service"
                          , Array [ "lambda.amazonaws.com" ]
                          )
                        ]
               )
             , ( "Action"
               , Array [ "sts:AssumeRole" ]
               )
             ]
    )
  ]

metaSpec :: SpecM () -> LambdaFunction Object () S.Resources
metaSpec spec = LambdaFunction $ const . return . Right $ describeSpecM spec
