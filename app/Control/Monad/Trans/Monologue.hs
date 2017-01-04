{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

module Control.Monad.Trans.Monologue
  ( MonologueT
  , MonadMonologue
  , runMonologueT
  , step
  , info
  , warn
  -- * Internals
  , MonologueT(MonologueT)
  ) where

--------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString                  as BS
import           Data.Monoid
import           Data.Text                        (Text)
import Prelude hiding (read)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as T
import           System.Console.ANSI
import Data.IORef
--------------------------------------------------------------------------------

indentation :: Int
indentation = 2

--------------------------------------------------------------------------------

newtype MonologueT m a = MonologueT (ReaderT (IORef MState) m a)
  deriving (Functor, Applicative, Monad)

deriving instance MonadThrow m => MonadThrow (MonologueT m)
deriving instance (MonadThrow m, MonadCatch m) => MonadCatch (MonologueT m)
deriving instance MonadIO m => MonadIO (MonologueT m) 

--------------------------------------------------------------------------------

data NoteType = Info | Warn

data MState
  = MState { _isOnNewLine :: Bool
           , _notes       :: [(NoteType, T.Text)]
           , _indentLevel :: Int
           }

read :: MonadIO m => MonologueT m MState 
read = MonologueT $ ask >>= liftIO . readIORef

modify :: MonadIO m => (MState -> MState) -> MonologueT m ()
modify f = MonologueT $ ask >>= liftIO . flip modifyIORef f

makeLenses ''MState

withColor :: MonadIO m
          => ColorIntensity -> Color
          -> MonologueT m a -> MonologueT m a
withColor intensity color ret
  = MonologueT (liftIO $ setSGR [SetColor Foreground intensity color])
     *> ret
     <* MonologueT (liftIO $ setSGR [Reset])

mAppendStr :: MonadIO m => Text -> MonologueT m ()
mAppendStr text = do
  MonologueT $ liftIO (BS.putStr $ T.encodeUtf8 text)
  modify (isOnNewLine .~ False)

mPutStr :: MonadIO m => Text -> MonologueT m ()
mPutStr text = do
  onNewLine <- view isOnNewLine <$> read
  unless onNewLine mEndLine
  indent
  mAppendStr text
  where
    indent = 
      (view indentLevel <$> read)
         >>= MonologueT . liftIO . putStr . flip replicate ' ' . (* indentation)

mEndLine :: MonadIO m => MonologueT m ()
mEndLine = do
  MonologueT $ liftIO $ putStrLn ""
  modify (isOnNewLine .~ True)

printNotes :: MonadIO m => MonologueT m ()
printNotes = do
  ns <- view notes <$> read
  forM_ ns $ \(ty, tx) -> do
    case ty of
      Info -> mPutStr ": "
      Warn -> mPutStr "! "
    mAppendStr tx
    mEndLine
  modify (notes .~ [])

step' :: (MonadCatch m, MonadIO m) => Text -> MonologueT m a -> MonologueT m a
step' name (MonologueT m) = do
  printNotes

  mPutStr $ "* " <> name

  modify (indentLevel +~ 1)
  ret <- MonologueT $ try m
  onNewLine <- view isOnNewLine <$> read
  unless onNewLine $ do
    case ret of
      Left  _ -> withColor Vivid Red $ mAppendStr "\t[FAIL]"
      Right _ -> withColor Vivid Green $ mAppendStr "\t[OK]"
    mEndLine

  printNotes
  modify (indentLevel -~ 1)

  case ret of
    Left err  -> throwM (err :: SomeException)
    Right val -> return val

note' :: MonadIO m => NoteType -> T.Text -> MonologueT m ()
note' ty text = modify (notes <>~ [(ty, text)])

info :: MonadMonologue m => T.Text -> m ()
info = note Info

warn :: MonadMonologue m => T.Text -> m ()
warn = note Warn

runMonologueT :: MonadIO m => MonologueT m a -> m a
runMonologueT (MonologueT t)
  = liftIO (newIORef $ MState True [] 0) >>= runReaderT t 

-- test :: MonologueT IO ()
-- test = do
--   step (T.pack "s1") $ return ()
--   step (T.pack "s2") $ return ()
--   step (T.pack "s3") $ do
--     step (T.pack "s3-1") $ do
--       step (T.pack "s3-1-2") $ do
--         error "utku"
--         return ()
--     step (T.pack "s3-2") $ return ()


--------------------------------------------------------------------------------

class MonadMonologue m where
  step :: Text -> m a -> m a
  note :: NoteType -> T.Text -> m ()

instance (MonadIO m, MonadCatch m) => MonadMonologue (MonologueT m) where
  step = step'
  note = note'
