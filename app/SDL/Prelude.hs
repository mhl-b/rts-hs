module SDL.Prelude where

import Control.Monad (unless, (>=>))
import Data.Int
import Data.Word
import Foreign (Storable (peek), alloca, toBool)
import Foreign.C (peekCString, withCString)
import Foreign.C.ConstPtr
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable (poke)
import SDL.FFI
import System.Exit (ExitCode (ExitFailure), exitWith)
import Vec

type FPoint = V2 Float

type FRect = V4 Float

type RGB = V3 Word8

type RGBA = V4 Word8

toCf32 :: (Functor f) => f Float -> f CFloat
toCf32 = fmap CFloat

toCu8 :: (Functor f) => f Word8 -> f CUChar
toCu8 = fmap CUChar

withConstStr :: String -> (ConstPtr CChar -> IO a) -> IO a
withConstStr str f = withCString str $ \cstr -> f (ConstPtr cstr)

sdlError :: IO String
sdlError = _error >>= peekCString . unConstPtr

printErrorAndQuit :: IO a
printErrorAndQuit = sdlError >>= print >> sdlQuit >> exitWith (ExitFailure 1)

handleCBoolErr :: CBool -> IO ()
handleCBoolErr cOk = unless (toBool cOk) printErrorAndQuit

handlePtrErr :: Ptr a -> IO (Ptr a)
handlePtrErr ptr = if ptr == nullPtr then printErrorAndQuit else return ptr

sdlInit :: IO ()
sdlInit = _init initFlags >>= handleCBoolErr

sdlQuit :: IO ()
sdlQuit = _quit

sdlWindow :: String -> Int32 -> Int32 -> Word64 -> IO SDLWindow
sdlWindow name width height flags = withConstStr name $ \cname ->
  _create_window cname (CInt width) (CInt height) (CULong flags) >>= handlePtrErr

sdlRenderer :: SDLWindow -> IO SDLRenderer
sdlRenderer window = do
  renderer <- _create_renderer window (ConstPtr nullPtr) >>= handlePtrErr
  _ <- _set_renderer_vsync renderer 1 >>= handleCBoolErr
  return renderer

sdlRenderClear :: SDLRenderer -> IO ()
sdlRenderClear = _renderer_clear >=> handleCBoolErr

sdlRenderPresent :: SDLRenderer -> IO ()
sdlRenderPresent = _renderer_present >=> handleCBoolErr

sdlPollEvents :: [SDLEvent] -> IO [SDLEvent]
sdlPollEvents evts = alloca $ \ptr -> do
  go ptr evts
  where
    go ptr' evts' = do
      ok <- _poll_event ptr'
      if toBool ok
        then
          peek ptr' >>= \evt -> case evt of
            Other -> go ptr' evts'
            _ -> go ptr' (evt : evts')
        else return evts'

type SDLTick = Word64

type SDLDuration = Word64

sdlTick :: IO SDLTick
sdlTick = fromIntegral <$> _get_ticks_ns

sdlSetDrawColorRGB :: SDLRenderer -> RGB -> IO ()
sdlSetDrawColorRGB rend rgb =
  let (V3 r g b) = toCu8 rgb
   in _set_render_draw_color rend r g b 0xFF >>= handleCBoolErr

sdlSetDrawColorBlack :: SDLRenderer -> IO ()
sdlSetDrawColorBlack r = sdlSetDrawColorRGB r (V3 0x00 0x00 0x00)

sdlSetDrawColorWhite :: SDLRenderer -> IO ()
sdlSetDrawColorWhite r = sdlSetDrawColorRGB r (V3 0xFF 0xFF 0xFF)

sdlRenderLine :: SDLRenderer -> FPoint -> FPoint -> IO ()
sdlRenderLine r p1 p2 =
  let (V2 x1 y1) = toCf32 p1
      (V2 x2 y2) = toCf32 p2
   in _render_line r x1 y1 x2 y2 >>= handleCBoolErr

sdlLoadPngTexture :: SDLRenderer -> String -> IO SDLTexture
sdlLoadPngTexture renderer path = withConstStr path $ \cpath -> do
  surface <- _load_png cpath >>= handlePtrErr
  texture <- _surface_to_texture renderer surface >>= handlePtrErr
  _ <- _destroy_surface surface
  return texture

withConstPtr :: (Storable a) => a -> (ConstPtr a -> IO b) -> IO b
withConstPtr s f = alloca $ \ptr -> poke ptr s >> f (ConstPtr ptr)

withConstPtr2 :: (Storable a) => a -> a -> (ConstPtr a -> ConstPtr a -> IO b) -> IO b
withConstPtr2 a b f = withConstPtr a $ \pa -> withConstPtr b $ \pb -> f pa pb

sdlRenderTexture :: SDLRenderer -> SDLTexture -> FRect -> FRect -> IO ()
sdlRenderTexture renderer texture src dst =
  withConstPtr2 (toCf32 src) (toCf32 dst) $ \srcPtr dstPtr ->
    _render_texture renderer texture srcPtr dstPtr >>= handleCBoolErr

sdlDestroyTexture :: SDLTexture -> IO ()
sdlDestroyTexture = _destroy_texture

sdlTTFInit :: IO ()
sdlTTFInit = _ttf_init >>= handleCBoolErr

sdlCreateTextEngine :: SDLRenderer -> IO SDLTextEngine
sdlCreateTextEngine = _create_text_engine >=> handlePtrErr

sdlOpenFont :: String -> Float -> IO SDLFont
sdlOpenFont path pt =
  withConstStr path $ \cpath -> _open_font cpath (CFloat pt) >>= handlePtrErr

sdlCreateText :: SDLTextEngine -> SDLFont -> String -> IO SDLText
sdlCreateText e f s =
  withConstStr s $ \cs -> _create_text e f cs (fromIntegral (length s)) >>= handlePtrErr

sdlDestroyText :: SDLText -> IO ()
sdlDestroyText = _destroy_text

sdlRenderText :: SDLText -> FPoint -> IO ()
sdlRenderText t p = let (V2 x y) = toCf32 p in _render_text t x y >>= handleCBoolErr
