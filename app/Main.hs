module Main where

import qualified SDL (sdlInit)

main :: IO ()
main = do
 ok <- SDL.sdlInit
 print ok
