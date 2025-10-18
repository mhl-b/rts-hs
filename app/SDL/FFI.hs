{-# LANGUAGE CApiFFI #-}

module SDL.FFI where

import Foreign.C
import Foreign.C.ConstPtr
import Foreign.Ptr
import SDL.Events

foreign import capi "SDL3/SDL.h SDL_Init" _init :: CUInt -> IO Bool

foreign import capi "SDL3/SDL.h SDL_Quit" _quit :: IO ()

foreign import capi "SDL3/SDL.h SDL_GetError" _error :: IO (ConstPtr CChar)

data RawWindow

newtype SDLWindow = SDLWindow (Ptr RawWindow)

foreign import capi "SDL3/SDL.h SDL_CreateWindow" _create_window :: ConstPtr CChar -> CInt -> CInt -> CUInt -> IO (Ptr RawWindow)

foreign import capi "SDL3/SDL.h SDL_PollEvent" _poll_event :: Ptr SDLEvent -> IO Bool