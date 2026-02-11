module Video where

import Consts (phi)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import SDL.FFI
import SDL.Prelude
import Vec
import Prelude hiding (lookup)
import Data.Word

data Video = Video
  { window :: SDLWindow,
    imageRenderer :: ImageRenderer,
    fontRenderer :: FontRenderer
  }

type AtlasMap = Map AtlasName Atlas

data ImageRenderer = ImageRenderer
  { renderer :: SDLRenderer,
    atlasMap :: AtlasMap
  }

data FontRenderer = FontRenderer
  { textEngine :: SDLTextEngine,
    smallFont :: SDLFont,
    mediumFont :: SDLFont
  }

initRenderer :: SDLWindow -> IO ImageRenderer
initRenderer window = do
  renderer <- sdlRenderer window
  return ImageRenderer {renderer, atlasMap = Map.empty}

fontPath :: String
fontPath = "data/font/courier.ttf"

initFont :: SDLRenderer -> IO FontRenderer
initFont renderer = do
  textEngine <- sdlCreateTextEngine renderer
  smallFont <- sdlOpenFont fontPath 12
  mediumFont <- sdlOpenFont fontPath 20
  return
    FontRenderer
      { textEngine,
        smallFont,
        mediumFont
      }

initVideo :: IO Video
initVideo = do
  window <- sdlWindow "app" 800 600 0
  imageRenderer <- initRenderer window
  fontRenderer <- initFont imageRenderer.renderer
  return
    Video
      { window,
        imageRenderer,
        fontRenderer
      }

class Drawable a where
  draw :: a -> Video -> IO Video

data AtlasName = ZlukAtlas deriving (Eq, Ord, Enum)

type AtlasSize = V2 Float

type AtlasIdx = V2 Float

data Atlas = Atlas
  { texture :: SDLTexture,
    w :: Float,
    h :: Float
  }
  deriving (Eq, Show)

imageBasePath :: String
imageBasePath = "data/images/"

loadAtlas :: SDLRenderer -> String -> AtlasSize -> IO Atlas
loadAtlas renderer name (V2 w h) = do
  texture <- sdlLoadPngTexture renderer (imageBasePath ++ name ++ ".png")
  return Atlas {texture, w, h}

loadAtlasByName :: SDLRenderer -> AtlasName -> IO Atlas
loadAtlasByName renderer i = case i of
  ZlukAtlas -> load "zluk" (V2 64 64)
  where
    load = loadAtlas renderer

loadAtlasByNameCached :: ImageRenderer -> AtlasName -> IO (ImageRenderer, Atlas)
loadAtlasByNameCached ir@ImageRenderer {..} atlasId =
  case Map.lookup atlasId atlasMap of
    Just atlas -> return (ir, atlas)
    Nothing -> do
      atlas <- loadAtlasByName renderer atlasId
      return (ir {atlasMap = Map.insert atlasId atlas atlasMap}, atlas)

atlasSrcFRect :: Float -> Float -> Float -> Float -> FRect
atlasSrcFRect w h xIdx yIdx = V4 (xIdx * w) (yIdx * h) w h

renderFromAtlas :: SDLRenderer -> Atlas -> AtlasIdx -> FPoint -> IO ()
renderFromAtlas renderer Atlas {..} (V2 xIdx yIdx) (V2 dstX dstY) = do
  let srcRect = atlasSrcFRect w h xIdx yIdx
  let dstRect = V4 dstX dstY w h
  sdlRenderTexture renderer texture srcRect dstRect

renderFromCachedAtlasById :: ImageRenderer -> AtlasName -> AtlasIdx -> FPoint -> IO ImageRenderer
renderFromCachedAtlasById ir atlasName atlasIdx dst = do
  (ir', atlas) <- loadAtlasByNameCached ir atlasName
  renderFromAtlas ir'.renderer atlas atlasIdx dst
  return ir'

floorf :: Float -> Float
floorf = fromInteger . floor


nanoSecToFloatSec :: Word64 -> Float
nanoSecToFloatSec ns =
  let ms = fromIntegral (ns `div` 1_000_000)
  in ms / 1000

-- f1 + f2 + f3 + f2 = f1 + 2 * phi * f1 + phi ^ 2 * f1
frameNumGolden3 :: Float -> Float -> Float
frameNumGolden3 toff tnow =
  let t = tnow - toff
      t' = t - floorf (t / 6.76) * 6.76
   in go t'
  where
    go x
      | x < 1 = 0
      | x < phi = 1
      | x < phi * phi = 2
      | otherwise = 1
