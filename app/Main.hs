module Main where

import Control.Concurrent.STM
import SDL.FFI
import SDL.Prelude
import Prelude

main :: IO ()
main = do
        _ <- sdlInit
        wr <- sdlWindow "coagula" 800 600 0
        case wr of
                Left err -> print err >> sdlQuit
                Right w -> do
                  tick <- getTick
                  eventsChan <- newTChanIO
                  drawChan <- newTChanIO
                  renderer <- sdlRenderer w
                  renderLoop RenderFrame{tick, renderer, eventsBuffer = [], eventsChan, drawChan}
                  sdlQuit

eventsLoop :: [SDLEvent] -> IO [SDLEvent]
eventsLoop es = do
        m <- sdlPollEvent
        case m of
                Nothing -> return es
                Just e -> eventsLoop (e : es)

data RenderFrame = RenderFrame
        { tick :: Tick
        , renderer :: SDLRenderer
        , eventsBuffer :: [SDLEvent]
        , eventsChan :: TChan [SDLEvent]
        , drawChan :: TChan [Point]
        }

renderLoop :: RenderFrame -> IO ()
renderLoop rf@RenderFrame{renderer, eventsBuffer, eventsChan} = do
        eventsBuffer' <- eventsLoop eventsBuffer
        let isQuit = Quit `elem` eventsBuffer'
        if isQuit
                then return ()
                else do
                        sdlRenderClear renderer
                        sdlRenderPresent renderer
                        renderLoop rf{eventsBuffer = eventsBuffer'}

type Point = (Int, Int)

data ComputeFrame = ComputeFrame
        { tick :: Tick
        , eventsChan :: TChan [SDLEvent]
        , drawChan :: TChan [Point]
        , n :: Int
        }

triangle :: [Point]
triangle = [(100,100), (300, 100), (200, 300)]

rectangle :: [Point]
rectangle = [(100,100), (300, 100), (300, 300), (100, 300)]

-- computeLoop :: ComputeFrame -> IO ()
-- computeLoop cf@ComputeFrame{tick, eventsChan, drawChan, n} = do return ()
