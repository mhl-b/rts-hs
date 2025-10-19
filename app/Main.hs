module Main where

import Control.Monad (unless, when)
import SDL.Events
import SDL.FFI
import SDL.Prelude
import Prelude

main :: IO ()
main = do
        _ <- sdlInit
        wr <- sdlWindow "coagula" 800 600 0
        case wr of
                Left err -> do
                        print err
                        sdlQuit
                Right w -> do
                        r <- sdlRenderer w
                        _ <- renderLoop r emptyFPS
                        sdlQuit

eventsLoop :: [SDLEvent] -> IO [SDLEvent]
eventsLoop es = do
        m <- sdlPollEvent
        case m of
                Nothing -> return es
                Just e -> eventsLoop (es ++ [e])

renderLoop :: SDLRenderer -> FPS -> IO ()
renderLoop r fps@(FPS{t0}) = do
        tick' <- getTick
        let fps' = updateFPS fps tick'
        es <- eventsLoop []
        _ <- maybePrint t0 tick' fps' es
        let isQuit = Quit `elem` es
        if isQuit
                then return ()
                else do
                        _ <- sdlRenderClear r
                        _ <- sdlRenderPresent r
                        renderLoop r fps'

maybePrint :: Tick -> Tick -> FPS -> [SDLEvent] -> IO ()
maybePrint t0 t1 fps events = do
        when (t1 - t0 >= 1000) $ do
                print fps
                print events
