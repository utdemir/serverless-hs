{-# LANGUAGE OverloadedStrings #-}

module Web.Serverless.Internal.Constants where

--------------------------------------------------------------------------------
import Data.Text
import Data.Monoid
--------------------------------------------------------------------------------

prefix :: Text
prefix = "serverlessHs"

parameter :: Text
parameter = prefix <> "Parameter"

parameterS3Bucket :: Text
parameterS3Bucket = parameter <> "S3bucket"

parameterS3Key :: Text
parameterS3Key = parameter <> "S3key"

resourceName :: Text
resourceName =  prefix <> "Resource"

--------------------------------------------------------------------------------

envFunctionId :: Text
envFunctionId = "SERVERLESS_HS_FUNCTION_ID"
