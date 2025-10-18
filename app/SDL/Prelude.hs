module SDL.Prelude (sdlInit, sdlQuit, sdlWindow, sdlPollEvent) where

import Foreign
import Foreign.C.ConstPtr
import Foreign.C.String
import SDL.Events
import SDL.FFI
import SDL.Init

type SDLErr = String

sdlErr :: IO SDLErr
sdlErr = do
  cerr <- _error
  peekCString (unConstPtr cerr)

catchErr :: IO (Either SDLErr a)
catchErr = Left <$> sdlErr

sdlInit :: IO (Either SDLErr ())
sdlInit = do
  ok <- _init initFlags
  if ok then return (Right ()) else catchErr

sdlQuit :: IO ()
sdlQuit = do _quit

sdlWindow :: String -> Int -> Int -> Int -> IO (Either SDLErr SDLWindow)
sdlWindow name w h flag = do
  cname <- newCString name
  ptr <- _create_window (ConstPtr cname) (fromIntegral w) (fromIntegral h) (fromIntegral flag)
  if ptr /= nullPtr then return (Right (SDLWindow ptr)) else catchErr

sdlPollEvent :: IO (Maybe SDLEvent)
sdlPollEvent = do
  alloca
    ( \ptr -> do
        ok <- _poll_event ptr
        if ok
          then do
            evt <- peek ptr
            return (Just evt)
          else
            return Nothing
    )