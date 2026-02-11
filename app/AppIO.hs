module AppIO where

import Control.Concurrent (threadDelay)
import SDL.FFI
import SDL.Prelude
import Text.Printf (printf)
import Vec
import Video

data AppIO = AppIO
  { video :: Video,
    input :: Input
  }

data Input = Input

initIO :: IO AppIO
initIO = do
  sdlInit
  sdlTTFInit
  video <- initVideo
  sdlSetDrawColorWhite video.imageRenderer.renderer
  sdlRenderClear video.imageRenderer.renderer
  let app = AppIO {video, input = Input}
  renderLoop app

readEvents :: [SDLEvent] -> IO [SDLEvent]
readEvents evts = filter (/= Other) <$> sdlPollEvents evts

renderLoop :: AppIO -> IO AppIO
renderLoop app = do
  ntick <- sdlTick
  evts <- readEvents []
  if Quit `elem` evts
    then return app
    else do
      threadDelay 20_000
      sdlRenderClear app.video.imageRenderer.renderer
      let s = nanoSecToFloatSec ntick
      let frame = frameNumGolden3 0.2 0 s
      printf "tick: %d s: %f frame: %f\n" ntick s frame
      ir <- renderFromCachedAtlasById app.video.imageRenderer ZlukAtlas (V2 frame 0) (V2 100 100)
      sdlRenderPresent app.video.imageRenderer.renderer
      let v' = app.video {imageRenderer = ir}
      renderLoop app {video = v'}
