module SDL.Init where

import Foreign.C
import Data.Bits

#include "SDL3/SDL.h"

initFlags :: CUInt
initFlags = foldr (.|.) 0 [#{const SDL_INIT_VIDEO}, #{const SDL_INIT_AUDIO}, #{const SDL_INIT_EVENTS}]