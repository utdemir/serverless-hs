{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Types.Args
  ( Args(..)
  ) where

--------------------------------------------------------------------------------
import qualified Filesystem.Path.CurrentOS as F
import Options.Generic
--------------------------------------------------------------------------------

data CLI
  = CLI (F.FilePath <?> "Path to lambda executable")
        (F.FilePath <?> "Path to JSON or YAML configuration.")
  deriving (Show, Generic)

instance ParseRecord CLI

--------------------------------------------------------------------------------

data Args
  = Args { executablePath  :: FilePath
         , configPath      :: FilePath
         }
  deriving Show

instance ParseRecord Args where
  parseRecord = toArgs <$> parseRecord
    where
      toArgs :: CLI -> Args
      toArgs (CLI (Helpful i) (Helpful o))
        = Args (F.encodeString i) (F.encodeString o)

