module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import SDL.FFI
import SDL.Prelude
import Text.Printf (printf)
import Prelude

main :: IO ()
main = do
  _ <- sdlInit
  window <- sdlWindow "coagula" 800 600 0
  tick <- getTick
  let fps = newFPS tick
  eventsChan <- newTChanIO
  computeChan <- newTChanIO
  renderer <- sdlRenderer window
  sdlSetDrawColorWhite renderer
  texture <- sdlLoadPngTexture renderer "data/images/zluk.png"
  _ <- forkOS $ computeLoop ComputeFrame {cfps = fps, eventsChan, computeChan, state = Triangle}
  renderLoop RenderFrame {rfps = fps, renderer, eventsChan, computeChan, texture, state = Triangle}
  sdlQuit

data RenderFrame = RenderFrame
  { rfps :: FPS,
    renderer :: SDLRenderer,
    eventsChan :: TChan [SDLEvent],
    computeChan :: TChan ComputeState,
    texture :: SDLTexture,
    state :: ComputeState
  }

animationAtlasPosition :: Tick -> SDLFRect
animationAtlasPosition tick =
  let frame = (tick `div` 250_000_000) `mod` 3
      x = 64 * fromIntegral frame
      y = 0
      w = 64
      h = 64
   in sdlFRect x y w h

renderLoop :: RenderFrame -> IO ()
renderLoop rf@RenderFrame {rfps, renderer, eventsChan, computeChan, texture, state} = do
  tick <- getTick
  fps' <- updateAndPrintFps "render" rfps
  events <- sdlPollEvents
  if Quit `elem` events
    then return ()
    else do
      unless (null events) $ atomically $ do writeTChan eventsChan events
      m <- atomically $ do tryReadTChan computeChan
      let state' = fromMaybe state m
      sdlRenderClear renderer
      sdlSetDrawColorBlack renderer
      drawShape renderer (computeStateToPoints state')
      sdlSetDrawColorWhite renderer
      sdlRenderTexture renderer texture (animationAtlasPosition tick) (sdlFRect 300 300 64 64)
      sdlRenderPresent renderer
      renderLoop rf {rfps = fps', state = state'}

data ComputeFrame = ComputeFrame
  { cfps :: FPS,
    eventsChan :: TChan [SDLEvent],
    computeChan :: TChan ComputeState,
    state :: ComputeState
  }

data ComputeState = Triangle | Rectangle

computeStateToPoints :: ComputeState -> [FPoint]
computeStateToPoints state = case state of
  Triangle -> triangle
  Rectangle -> rectangle

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
      m <- tryReadTChan ch
      case m of
        Nothing -> return acc
        Just item -> drainTChan' ch (item : acc)

computeLoop :: ComputeFrame -> IO ()
computeLoop cf@ComputeFrame {cfps, eventsChan, computeChan, state} = do
  threadDelay 100_000
  fps' <- updateAndPrintFps "compute" cfps
  evs <- drainTChan eventsChan
  let evs' = concatMap (filter (== MouseDown)) evs
  if not (null evs')
    then case state of
      Triangle -> do
        atomically $ writeTChan computeChan Triangle
        computeLoop cf {cfps = fps', state = Rectangle}
      Rectangle -> do
        atomically $ writeTChan computeChan Rectangle
        computeLoop cf {cfps = fps', state = Triangle}
    else computeLoop cf {cfps = fps'}

data FPS = FPS {frameStart :: Tick, totalFrames :: Int}

newFPS :: Tick -> FPS
newFPS frameStart = FPS {frameStart, totalFrames = 0}

updateAndPrintFps :: String -> FPS -> IO FPS
updateAndPrintFps prefix FPS {frameStart, totalFrames} = do
  tick <- getTick
  if tick - frameStart >= 1_000_000_000
    then do
      printf "%s fps: %d\n" prefix (totalFrames + 1)
      return FPS {frameStart = tick, totalFrames = 0}
    else
      return FPS {frameStart, totalFrames = totalFrames + 1}
