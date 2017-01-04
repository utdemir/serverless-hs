{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Web.Serverless.Types.Args
  ( Args (..)
  ) where

--------------------------------------------------------------------------------
import qualified Filesystem.Path.CurrentOS as F
import           Prelude
--------------------------------------------------------------------------------
import           Options.Generic
--------------------------------------------------------------------------------

data CLI
  = CLI (F.FilePath <?> "File to read events")
        (F.FilePath <?> "File to write answers")
  deriving (Show, Generic)

instance ParseRecord CLI

--------------------------------------------------------------------------------

data Args
  = Args { inputFilename  :: FilePath
         , outputFilename :: FilePath
         }
  deriving Show

instance ParseRecord Args where
  parseRecord = toArgs <$> parseRecord
    where
      toArgs :: CLI -> Args
      toArgs (CLI (Helpful i) (Helpful o))
        = Args (F.encodeString i) (F.encodeString o)

