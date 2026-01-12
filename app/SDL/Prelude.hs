module SDL.Prelude where

import Control.Monad (unless)
import Data.Word (Word64, Word8)
import Foreign (Storable (peek), alloca, toBool)
import Foreign.C (newCString, peekCString)
import Foreign.C.ConstPtr
import Foreign.C.Types
import Foreign.Ptr
import SDL.FFI
import System.Exit (ExitCode (ExitFailure), exitWith)

stringToConstPtr :: String -> IO (ConstPtr CChar)
stringToConstPtr s = ConstPtr <$> newCString s

type SDLErr = String

sdlErr :: IO SDLErr
sdlErr = _error >>= (peekCString . unConstPtr)

printErrorAndQuit :: IO a
printErrorAndQuit = _error >>= print >> sdlQuit >> exitWith (ExitFailure 1)

handleBoolErr :: Bool -> IO ()
handleBoolErr ok = unless ok printErrorAndQuit

handleCBoolErr :: CBool -> IO ()
handleCBoolErr cOk = handleBoolErr (toBool cOk)

handlePtrErr :: Ptr a -> IO (Ptr a)
handlePtrErr ptr = if ptr == nullPtr then printErrorAndQuit else return ptr

sdlInit :: IO ()
sdlInit = _init initFlags >>= handleCBoolErr

sdlQuit :: IO ()
sdlQuit = _quit

sdlWindow :: String -> Int -> Int -> Word64 -> IO SDLWindow
sdlWindow name width height flags = do
  cname <- stringToConstPtr name
  ptr <- _create_window cname (fromIntegral width) (fromIntegral height) (fromIntegral flags)
  handlePtrErr ptr

sdlRenderer :: SDLWindow -> IO SDLRenderer
sdlRenderer w = _create_renderer w (ConstPtr nullPtr) >>= handlePtrErr

sdlVSync :: SDLRenderer -> IO ()
sdlVSync r = _set_renderer_vsync r 1 >>= handleCBoolErr

sdlRenderClear :: SDLRenderer -> IO ()
sdlRenderClear r = _renderer_clear r >>= handleCBoolErr

sdlRenderPresent :: SDLRenderer -> IO ()
sdlRenderPresent r = _renderer_present r >>= handleCBoolErr

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
   in _set_render_draw_color rend r' g' b' a' >>= handleCBoolErr

data V2 s = V2 !s !s deriving (Eq, Show, Functor)

type FPoint = V2 Float

type SDLPoint = V2 CFloat

sdlRenderLine :: SDLRenderer -> FPoint -> FPoint -> IO ()
sdlRenderLine r p1 p2 =
  let (V2 x1 y1) = fmap realToFrac p1
      (V2 x2 y2) = fmap realToFrac p2
   in _render_line r x1 y1 x2 y2 >>= handleCBoolErr
