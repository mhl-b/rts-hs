module Main where

import Control.Concurrent
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
    Right window -> do
      tick <- getTick
      eventsChan <- newTChanIO
      drawChan <- newTChanIO
      renderer <- sdlRenderer window
      _ <- forkOS $ computeLoop ComputeFrame {tick, eventsChan, drawChan, state = Triangle}
      renderLoop RenderFrame {tick, renderer, eventsBuffer = [], eventsChan, drawChan}
      sdlQuit

eventsLoop :: [SDLEvent] -> IO [SDLEvent]
eventsLoop es = do
  m <- sdlPollEvent
  case m of
    Nothing -> return es
    Just e -> eventsLoop (e : es)

data RenderFrame = RenderFrame
  { tick :: Tick,
    renderer :: SDLRenderer,
    eventsBuffer :: [SDLEvent],
    eventsChan :: TChan [SDLEvent],
    drawChan :: TChan [FPoint]
  }

renderLoop :: RenderFrame -> IO ()
renderLoop rf@RenderFrame {renderer, eventsBuffer, eventsChan, drawChan} = do
  eventsBuffer' <- eventsLoop eventsBuffer
  let isQuit = Quit `elem` eventsBuffer'
  if isQuit
    then return ()
    else do
      atomically $ do writeTChan eventsChan eventsBuffer'
      m <- atomically $ do tryReadTChan drawChan
      case m of
        Nothing -> renderLoop rf {eventsBuffer = []}
        Just shape -> do
          sdlRenderClear renderer
          sdlSetDrawColor renderer 0xFF 0xFF 0xFF 0xFF
          drawShape renderer shape
          sdlSetDrawColor renderer 0x00 0x00 0x00 0x00
          sdlRenderPresent renderer
          renderLoop rf {eventsBuffer = []}

data ComputeFrame = ComputeFrame
  { tick :: Tick,
    eventsChan :: TChan [SDLEvent],
    drawChan :: TChan [FPoint],
    state :: ComputeState
  }

data ComputeState = Triangle | Rectangle

triangle :: [FPoint]
triangle = [V2 100 100, V2 300 100, V2 200 300]

rectangle :: [FPoint]
rectangle = [V2 100 100, V2 300 100, V2 300 300, V2 100 300]

drawShape :: SDLRenderer -> [FPoint] -> IO ()
drawShape r pts =
  let pairs = zip pts (tail pts) ++ [(head pts, last pts)]
   in do mapM_ (uncurry (sdlRenderLine r)) pairs

drainTChan :: TChan a -> IO [a]
drainTChan chan = atomically $ drainTChan' chan []
  where
    drainTChan' :: TChan a -> [a] -> STM [a]
    drainTChan' ch acc = do
      isEmpty <- isEmptyTChan ch
      if isEmpty
        then return (reverse acc)
        else do
          item <- readTChan ch
          drainTChan' ch (item : acc)

computeLoop :: ComputeFrame -> IO ()
computeLoop cf@ComputeFrame {tick, eventsChan, drawChan, state} = do
  threadDelay 100_000
  evs <- drainTChan eventsChan
  let evs' = concatMap (filter (== MouseDown)) evs
  print evs'
  if not (null evs')
    then case state of
      Triangle -> do
        atomically $ writeTChan drawChan triangle
        computeLoop cf {state = Rectangle}
      Rectangle -> do
        atomically $ writeTChan drawChan rectangle
        computeLoop cf {state = Triangle}
    else computeLoop cf
