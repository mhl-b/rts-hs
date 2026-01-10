module SDL.Prelude where

import Foreign
import Foreign.C (CChar, newCString, peekCString)
import Foreign.C.ConstPtr
import SDL.FFI

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
        if toBool ok then return (Right ()) else catchErr

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
                        if toBool ok
                                then do
                                        evt <- peek ptr
                                        return (Just evt)
                                else
                                        return Nothing
                )

type Tick = Word64

getTick :: IO Tick
getTick = fromIntegral <$> _get_ticks_ns

data V2 s = V2 !s !s deriving (Eq, Show)

type FPoint = V2 Float

-- sdlRenderLine :: SDLRenderer -> FPoint -> FPoint -> IO (Maybe SDLErr)
-- sdlRenderLine r FPoint x1 y1 FPoint x2 y2 = 
