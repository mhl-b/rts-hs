module SDL.Prelude where

import Control.Monad (void)
import Data.Word (Word64, Word8)
import Foreign (Storable (peek), alloca, nullPtr, toBool)
import Foreign.C (CChar, newCString, peekCString)
import Foreign.C.ConstPtr
import Foreign.C.Types
import Foreign.Ptr
import SDL.FFI

stringToConstPtr :: String -> IO (ConstPtr CChar)
stringToConstPtr s = ConstPtr <$> newCString s

type SDLErr = String

sdlErr :: IO SDLErr
sdlErr = _error >>= (peekCString . unConstPtr)

checkNullPtr :: IO (Ptr a) -> IO (Either SDLErr (Ptr a))
checkNullPtr action = action >>= (\ptr -> if ptr == nullPtr then Left <$> sdlErr else return (Right ptr))

maybeErr :: IO CBool -> IO (Maybe SDLErr)
maybeErr io = io >>= (\ok -> if toBool ok then return Nothing else Just <$> sdlErr)

sdlInit :: IO (Maybe SDLErr)
sdlInit = maybeErr (_init initFlags)

sdlQuit :: IO ()
sdlQuit = _quit

sdlWindow :: String -> Int -> Int -> Word64 -> IO (Either SDLErr SDLWindow)
sdlWindow name w h flag = do
  cname <- stringToConstPtr name
  checkNullPtr (_create_window cname (fromIntegral w) (fromIntegral h) (fromIntegral flag))

sdlRenderer :: SDLWindow -> IO (Either SDLErr SDLRenderer)
sdlRenderer w = checkNullPtr (_create_renderer w (ConstPtr nullPtr))

sdlVSync :: SDLRenderer -> IO (Maybe SDLErr)
sdlVSync r = maybeErr (_set_renderer_vsync r 1)

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

sdlSetDrawColor :: SDLRenderer -> Word8 -> Word8 -> Word8 -> Word8 -> IO (Maybe SDLErr)
sdlSetDrawColor rend r g b a =
  let r' = fromIntegral r
      g' = fromIntegral g
      b' = fromIntegral b
      a' = fromIntegral a
   in maybeErr (_set_render_draw_color rend r' g' b' a')

data V2 s = V2 !s !s deriving (Eq, Show, Functor)

type FPoint = V2 Float

type SDLPoint = V2 CFloat

sdlRenderLine :: SDLRenderer -> FPoint -> FPoint -> IO (Maybe SDLErr)
sdlRenderLine r p1 p2 =
  let (V2 x1 y1) = fmap realToFrac p1
      (V2 x2 y2) = fmap realToFrac p2
   in maybeErr (_render_line r x1 y1 x2 y2)
