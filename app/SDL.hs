{-# LANGUAGE CApiFFI #-}

module SDL (sdlInit) where

import Data.Bits
import Foreign.C (peekCString)
import Foreign.C.ConstPtr
import Foreign.C.Types

foreign import capi "SDL3/SDL.h SDL_Init" c_sdl_init :: CUInt -> IO Bool

foreign import capi "SDL3/SDL.h SDL_GetError" c_sdl_get_error :: IO (ConstPtr CChar)

foreign import capi "SDL3/SDL.h value SDL_INIT_AUDIO" c_sdl_init_audio :: CUInt

foreign import capi "SDL3/SDL.h value SDL_INIT_VIDEO" c_sdl_init_video :: CUInt

foreign import capi "SDL3/SDL.h value SDL_INIT_EVENTS" c_sdl_init_events :: CUInt

type SDLErr = String

sdlErr :: IO SDLErr
sdlErr = do
  cerr <- c_sdl_get_error
  peekCString (unConstPtr cerr)

sdlInit :: IO (Maybe SDLErr)
sdlInit = do
  ok <- c_sdl_init (c_sdl_init_video .|. c_sdl_init_events .|. c_sdl_init_audio)
  if ok
    then return Nothing
    else do Just <$> sdlErr
