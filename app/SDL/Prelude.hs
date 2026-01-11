module SDL.Prelude where

import Control.Monad (void)
import Data.Word (Word64, Word8)
import Foreign (Storable (peek), alloca, nullPtr, toBool)
import Foreign.C (CChar, newCString, peekCString)
import Foreign.C.ConstPtr
import Foreign.C.Types
import SDL.FFI

stringToConstPtr :: String -> IO (ConstPtr CChar)
stringToConstPtr s = do
  cs <- newCString s
  return (ConstPtr cs)

type SDLErr = String

sdlErr :: IO SDLErr
sdlErr = _error >>= (peekCString . unConstPtr)

eitherErr :: IO (Either SDLErr a)
eitherErr = Left <$> sdlErr

maybeErr :: IO CBool -> IO (Maybe SDLErr)
maybeErr io = io >>= (\ok -> if toBool ok then return Nothing else Just <$> sdlErr)

sdlInit :: IO (Either SDLErr ())
sdlInit = do
  ok <- _init initFlags
  if toBool ok then return (Right ()) else eitherErr

sdlQuit :: IO ()
sdlQuit = _quit

sdlWindow :: String -> Int -> Int -> Int -> IO (Either SDLErr SDLWindow)
sdlWindow name w h flag = do
  cname <- stringToConstPtr name
  ptr <- _create_window cname (fromIntegral w) (fromIntegral h) (fromIntegral flag)
  if ptr /= nullPtr then return (Right (SDLWindow ptr)) else eitherErr

sdlRenderer :: SDLWindow -> IO SDLRenderer
sdlRenderer w = do
  r <- _create_renderer w (ConstPtr nullPtr)
  _ <- _set_renderer_vsync r 1
  return r

sdlRenderClear :: SDLRenderer -> IO ()
sdlRenderClear r = void (_renderer_clear r)

sdlRenderPresent :: SDLRenderer -> IO ()
sdlRenderPresent r = void (_renderer_present r)

sdlPollEvent :: IO (Maybe SDLEvent)
sdlPollEvent = alloca $ \ptr -> do
  ok <- _poll_event ptr
  if toBool ok
    then Just <$> peek ptr
    else return Nothing

type Tick = Word64

getTick :: IO Tick
getTick = fromIntegral <$> _get_ticks_ns

sdlSetDrawColor :: SDLRenderer -> Word8 -> Word8 -> Word8 -> Word8 -> IO ()
sdlSetDrawColor rend r g b a =
  let r' = fromIntegral r
      g' = fromIntegral g
      b' = fromIntegral b
      a' = fromIntegral a
   in void (_set_render_draw_color rend r' g' b' a')

data V2 s = V2 !s !s deriving (Eq, Show, Functor)

type FPoint = V2 Float

type SDLPoint = V2 CFloat

sdlRenderLine :: SDLRenderer -> FPoint -> FPoint -> IO ()
sdlRenderLine r p1 p2 =
  let (V2 x1 y1) = fmap realToFrac p1
      (V2 x2 y2) = fmap realToFrac p2
   in void (_render_line r x1 y1 x2 y2)
