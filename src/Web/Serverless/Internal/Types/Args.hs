{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Web.Serverless.Internal.Types.Args
  ( Args (..)
  ) where

--------------------------------------------------------------------------------
import           Options.Generic
import Data.Text (Text)
--------------------------------------------------------------------------------

data CLI
  = CLI (Int <?> "Port to listen on")
  deriving (Show, Generic)

instance ParseRecord CLI

--------------------------------------------------------------------------------

data Args
  = Args { argsPort  :: Int
         }
  deriving Show

instance ParseRecord Args where
  parseRecord = toArgs <$> parseRecord
    where
      toArgs :: CLI -> Args
      toArgs (CLI (Helpful i)) = Args i


