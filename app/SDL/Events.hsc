module SDL.Events where

import Foreign
import Foreign.C

#include "SDL3/SDL.h"

data SDLEvent = KeyDown | KeyUp | Quit | Other deriving (Eq, Show)

peekEvenType :: Ptr SDLEvent -> IO CUInt
peekEvenType ptr = do #{peek SDL_Event, type} ptr

pokeEventType :: Ptr SDLEvent -> CUInt-> IO ()
pokeEventType ptr ctype = #{poke SDL_Event, type} ptr ctype

instance Storable SDLEvent where
    sizeOf _ = #{size SDL_Event}
    alignment _ = #{alignment SDL_Event}

    peek ptr = do
        etype <- peekEvenType ptr
        return $ case etype of 
            #{const SDL_EVENT_KEY_DOWN} -> KeyDown
            #{const SDL_EVENT_KEY_UP} -> KeyUp
            #{const SDL_EVENT_QUIT} -> Quit
            _ -> Other
    
    poke ptr _ = do
        pokeEventType ptr 0
