module SDL.Prelude where

import Foreign
import Foreign.C
import Foreign.C.ConstPtr
import SDL.Events
import SDL.FFI
import SDL.Init

stringToConstPtr :: String -> IO (ConstPtr CChar)
stringToConstPtr s = do
        cs <- newCString s
        return (ConstPtr cs)

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
        cname <- stringToConstPtr name
        ptr <- _create_window cname (fromIntegral w) (fromIntegral h) (fromIntegral flag)
        if ptr /= nullPtr then return (Right (SDLWindow ptr)) else catchErr

sdlRenderer :: SDLWindow -> IO SDLRenderer
sdlRenderer w = do
        r <- _create_renderer w (ConstPtr nullPtr)
        _ <- _set_renderer_vsync r 1
        return r

sdlRenderClear :: SDLRenderer -> IO ()
sdlRenderClear r = do
        _ <- _renderer_clear r
        return ()

sdlRenderPresent :: SDLRenderer -> IO ()
sdlRenderPresent r = do
        _ <- _renderer_present r
        return ()

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

data FPS = FPS {past :: Int, now :: Int, t0 :: Tick} deriving (Show)

emptyFPS :: FPS
emptyFPS = FPS{past = 0, now = 0, t0 = 0}

updateFPS :: FPS -> Tick -> FPS
updateFPS FPS{past, now, t0} t1 =
        if t1 - t0 >= 1000
                then FPS{past = now, now = 1, t0 = t1}
                else FPS{past, now = now + 1, t0}

type Tick = Int64

type TickNS = Int64

getTick :: IO Tick
getTick = do fromIntegral <$> _get_ticks

getTickNS :: IO TickNS
getTickNS = do fromIntegral <$> _get_ticks_ns
