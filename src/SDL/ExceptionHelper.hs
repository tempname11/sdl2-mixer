module SDL.ExceptionHelper where

import Control.Exception       (throwIO)
import Control.Monad           (when)
import Control.Monad.IO.Class  (MonadIO, liftIO)
import Data.ByteString         (packCString)
import Data.Text               (Text)
import Data.Text.Encoding      (decodeUtf8)
import Foreign.Ptr             (Ptr, nullPtr)
import SDL                     (SDLException(SDLCallFailed))
import SDL.Raw                 (getError)

throwIfNeg_ :: (MonadIO m, Num a, Ord a) => Text -> Text -> m a -> m ()
throwIfNeg_ = throwIf_ (< 0)

throwIfNeg :: (MonadIO m, Num a, Ord a) => Text -> Text -> m a -> m a
throwIfNeg = throwIf (< 0)

throwIf0 :: (MonadIO m, Num a, Eq a) => Text -> Text -> m a -> m a
throwIf0 = throwIf (== 0)

throwIfNull :: MonadIO m => Text -> Text -> m (Ptr a) -> m (Ptr a)
throwIfNull = throwIf (== nullPtr)

throwIf :: MonadIO m => (a -> Bool) -> Text -> Text -> m a -> m a
throwIf f caller rawf act = do
  result <- act
  liftIO $ when (f result) $ do
    throwFailed caller rawf
  return result

throwIf_ :: MonadIO m => (a -> Bool) -> Text -> Text -> m a -> m ()
throwIf_ f caller rawf act = do
  _ <- throwIf f caller rawf act
  return ()

-- Allows us to just throw an SDL exception directly.
throwFailed :: MonadIO m => Text -> Text -> m a
throwFailed caller rawf = liftIO $ do
  err <- decodeUtf8 <$> (packCString =<< getError)
  throwIO $ SDLCallFailed caller rawf err
