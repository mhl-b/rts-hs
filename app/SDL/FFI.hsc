{-# LANGUAGE CApiFFI #-}

module SDL.FFI where

import Vec
import Data.Word
import Foreign
import Foreign.C
import Foreign.C.ConstPtr

#include "SDL3/SDL.h"

type RawPtr = Ptr ()

type SDLWindow = RawPtr

type SDLWindowFlags = CULong

type SDLRenderer = RawPtr

type SDLFPoint = V2 CFloat

type SDLFRect = V4 CFloat

type SDLSurface = RawPtr

type SDLTexture = RawPtr

type SDLTextEngine = RawPtr

type SDLFont = RawPtr

type SDLText = RawPtr

data SDLEvent = KeyDown | KeyUp | MouseUp | MouseDown | Quit | Other deriving (Eq, Show)

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

initFlags :: CUInt
initFlags = (#const SDL_INIT_VIDEO) .|. (#const SDL_INIT_AUDIO) .|. (#const SDL_INIT_EVENTS)

foreign import capi "SDL3/SDL.h SDL_Init"
  _init :: CUInt -> IO CBool

foreign import capi "SDL3/SDL.h SDL_Quit"
  _quit :: IO ()

foreign import capi "SDL3/SDL.h SDL_GetError"
  _error :: IO (ConstPtr CChar)

foreign import capi "SDL3/SDL.h SDL_CreateWindow"
  _create_window :: ConstPtr CChar -> CInt -> CInt -> CULong -> IO SDLWindow

foreign import capi "SDL3/SDL.h SDL_GetRenderer"
  _get_renderer :: SDLWindow -> IO SDLRenderer

foreign import capi "SDL3/SDL.h SDL_CreateRenderer"
  _create_renderer :: SDLWindow -> ConstPtr CChar -> IO SDLRenderer

foreign import capi "SDL3/SDL.h SDL_SetRenderVSync"
  _set_renderer_vsync :: SDLRenderer -> CInt -> IO CBool

foreign import capi "SDL3/SDL.h SDL_RenderClear"
  _renderer_clear :: SDLRenderer -> IO CBool

foreign import capi "SDL3/SDL.h SDL_RenderPresent"
  _renderer_present :: SDLRenderer -> IO CBool

foreign import capi "SDL3/SDL.h SDL_PollEvent"
  _poll_event :: Ptr SDLEvent -> IO CBool

foreign import capi "SDL3/SDL.h SDL_GetTicks"
  _get_ticks :: IO CULLong

foreign import capi "SDL3/SDL.h SDL_GetTicksNS"
  _get_ticks_ns :: IO CULLong

foreign import capi "SDL3/SDL.h SDL_RenderLine"
  _render_line :: SDLRenderer -> CFloat -> CFloat -> CFloat -> CFloat -> IO CBool

foreign import capi "SDL3/SDL.h SDL_SetRenderDrawColor"
  _set_render_draw_color :: SDLRenderer -> CChar -> CChar -> CChar -> CChar -> IO CBool

foreign import capi "SDL3/SDL.h SDL_LoadPNG"
  _load_png :: ConstPtr CChar -> IO SDLSurface

foreign import capi "SDL3/SDL.h SDL_CreateTextureFromSurface"
  _surface_to_texture :: SDLRenderer -> SDLSurface -> IO SDLTexture

foreign import capi "SDL3/SDL.h SDL_DestroySurface"
  _destroy_surface :: SDLSurface -> IO ()

foreign import capi "SDL3/SDL.h SDL_RenderTexture"
  _render_texture :: SDLRenderer -> SDLTexture -> ConstPtr SDLFRect -> ConstPtr SDLFRect -> IO CBool

foreign import capi "SDL3/SDL.h SDL_DestroyTexture"
  _destroy_texture :: SDLTexture -> IO ()

foreign import capi "SDL3_ttf/SDL_ttf.h TTF_Init"
  _ttf_init :: IO CBool

foreign import capi "SDL3_ttf/SDL_ttf.h TTF_CreateRendererTextEngine"
  _create_text_engine :: SDLRenderer -> IO SDLTextEngine

foreign import capi "SDL3_ttf/SDL_ttf.h TTF_OpenFont"
  _open_font :: ConstPtr CChar -> CFloat -> IO SDLFont

foreign import capi "SDL3_ttf/SDL_ttf.h TTF_CreateText"
  _create_text :: SDLTextEngine -> SDLFont -> ConstPtr CChar -> CSize -> IO SDLText

foreign import capi "SDL3_ttf/SDL_ttf.h TTF_DestroyText"
  _destroy_text :: SDLText -> IO ()

foreign import capi "SDL3_ttf/SDL_ttf.h TTF_DrawRendererText"
  _render_text :: SDLText -> CFloat -> CFloat -> IO CBool
