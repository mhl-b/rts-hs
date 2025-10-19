{-# LANGUAGE CApiFFI #-}

module SDL.FFI where

import Data.Word
import Foreign.C
import Foreign.C.ConstPtr
import Foreign.Ptr
import SDL.Events

foreign import capi "SDL3/SDL.h SDL_Init" _init :: CUInt -> IO Bool

foreign import capi "SDL3/SDL.h SDL_Quit" _quit :: IO ()

foreign import capi "SDL3/SDL.h SDL_GetError" _error :: IO (ConstPtr CChar)

data Raw

newtype SDLWindow = SDLWindow (Ptr Raw) deriving (Show)

newtype SDLRenderer = SDLRenderer (Ptr Raw) deriving (Show)

foreign import capi "SDL3/SDL.h SDL_CreateWindow" _create_window :: ConstPtr CChar -> CInt -> CInt -> CUInt -> IO (Ptr Raw)

foreign import capi "SDL3/SDL.h SDL_GetRenderer" _get_renderer :: SDLWindow -> IO SDLRenderer

foreign import capi "SDL3/SDL.h SDL_CreateRenderer" _create_renderer :: SDLWindow -> ConstPtr CChar -> IO SDLRenderer

foreign import capi "SDL3/SDL.h SDL_SetRenderVSync" _set_renderer_vsync :: SDLRenderer -> CInt -> IO Bool

foreign import capi "SDL3/SDL.h SDL_RenderClear" _renderer_clear :: SDLRenderer -> IO Bool

foreign import capi "SDL3/SDL.h SDL_RenderPresent" _renderer_present :: SDLRenderer -> IO Bool

foreign import capi "SDL3/SDL.h SDL_PollEvent" _poll_event :: Ptr SDLEvent -> IO Bool

foreign import capi "SDL3/SDL.h SDL_GetTicks" _get_ticks :: IO Word64

foreign import capi "SDL3/SDL.h SDL_GetTicksNS" _get_ticks_ns :: IO Word64
