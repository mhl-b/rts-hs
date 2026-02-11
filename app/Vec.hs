module Vec where

import Foreign

data V2 a = V2 !a !a deriving (Eq, Show, Functor, Foldable)

data V3 a = V3 !a !a !a deriving (Eq, Show, Functor, Foldable)

data V4 a = V4 !a !a !a !a deriving (Eq, Show, Functor, Foldable)

instance (Storable a) => Storable (V4 a) where
  sizeOf _ = 4 * sizeOf (undefined :: a)
  alignment _ = alignment (undefined :: a)
  peek ptr = do
    a <- peekElemOff (castPtr ptr) 0
    b <- peekElemOff (castPtr ptr) 1
    c <- peekElemOff (castPtr ptr) 2
    d <- peekElemOff (castPtr ptr) 3
    return (V4 a b c d)
  poke ptr (V4 a b c d) = do
    pokeElemOff (castPtr ptr) 0 a
    pokeElemOff (castPtr ptr) 1 b
    pokeElemOff (castPtr ptr) 2 c
    pokeElemOff (castPtr ptr) 3 d
