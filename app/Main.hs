module Main where

import Control.Monad (unless)
import SDL.Events
import SDL.Prelude (sdlInit, sdlPollEvent, sdlQuit, sdlWindow)
import Prelude

main :: IO ()
main = do
  _ <- sdlInit
  _ <- sdlWindow "coagula" 800 600 0
  _ <- renderLoop
  sdlQuit

eventsLoop :: [SDLEvent] -> IO [SDLEvent]
eventsLoop es = do
  m <- sdlPollEvent
  case m of
    Nothing -> return es
    Just e -> eventsLoop (es ++ [e])

renderLoop :: IO ()
renderLoop = do
  es <- eventsLoop []
  _ <- maybePrint es
  let isQuit = Quit `elem` es
  unless isQuit renderLoop

maybePrint :: [SDLEvent] -> IO ()
maybePrint es = do
  unless (null es) $ print es