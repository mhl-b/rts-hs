module Main where

import qualified MyLib (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  putStrLn "Bye"
  MyLib.someFunc
