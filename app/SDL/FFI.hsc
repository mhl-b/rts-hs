{-# LANGUAGE CApiFFI #-}

module SDL.FFI where

import Data.Word
import Foreign
import Foreign.C
import Foreign.C.ConstPtr

#include "SDL3/SDL.h"

initFlags :: CUInt
initFlags = foldr (.|.) 0 [(#const SDL_INIT_VIDEO), (#const SDL_INIT_AUDIO), (#const SDL_INIT_EVENTS)]

foreign import capi "SDL3/SDL.h SDL_Init" _init :: CUInt -> IO CBool

foreign import capi "SDL3/SDL.h SDL_Quit" _quit :: IO ()

foreign import capi "SDL3/SDL.h SDL_GetError" _error :: IO (ConstPtr CChar)

data Raw

type SDLWindow = Ptr Raw

type SDLWindowFlags = Word64

type SDLRenderer = Ptr Raw

foreign import capi "SDL3/SDL.h SDL_CreateWindow" _create_window :: ConstPtr CChar -> CInt -> CInt -> CULong -> IO SDLWindow

foreign import capi "SDL3/SDL.h SDL_GetRenderer" _get_renderer :: SDLWindow -> IO SDLRenderer

foreign import capi "SDL3/SDL.h SDL_CreateRenderer" _create_renderer :: SDLWindow -> ConstPtr CChar -> IO SDLRenderer

foreign import capi "SDL3/SDL.h SDL_SetRenderVSync" _set_renderer_vsync :: SDLRenderer -> CInt -> IO CBool

foreign import capi "SDL3/SDL.h SDL_RenderClear" _renderer_clear :: SDLRenderer -> IO CBool

foreign import capi "SDL3/SDL.h SDL_RenderPresent" _renderer_present :: SDLRenderer -> IO CBool

data SDLEvent = KeyDown | KeyUp | MouseUp | MouseDown | Quit | Other deriving (Eq, Show)

foreign import capi "SDL3/SDL.h SDL_PollEvent" _poll_event :: Ptr SDLEvent -> IO CBool

instance Storable SDLEvent where
    sizeOf _ = (#size SDL_Event)
    alignment _ = (#alignment SDL_Event)

    peek ptr = do
        etype :: CUInt <- (#peek SDL_Event, type) ptr
        return $ case etype of 
            (#const SDL_EVENT_KEY_DOWN) -> KeyDown
            (#const SDL_EVENT_MOUSE_BUTTON_DOWN) -> MouseDown
            (#const SDL_EVENT_QUIT) -> Quit
            _ -> Other
    
    poke _ _ = undefined

foreign import capi "SDL3/SDL.h SDL_GetTicks" _get_ticks :: IO CULLong

foreign import capi "SDL3/SDL.h SDL_GetTicksNS" _get_ticks_ns :: IO CULLong

foreign import capi "SDL3/SDL.h SDL_RenderLine" _render_line :: SDLRenderer -> CFloat -> CFloat -> CFloat -> CFloat -> IO CBool

foreign import capi "SDL3/SDL.h SDL_SetRenderDrawColor" _set_render_draw_color :: SDLRenderer -> CChar -> CChar -> CChar -> CChar -> IO CBool
