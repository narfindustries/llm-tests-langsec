module GIF.GifGemini1 where

import Daedalus.Maybe
import Daedalus.Text

data GifGemini1 = GifGemini1 {
  header ::  Maybe (Array 3 Byte),
  width :: Word32,
  height :: Word32,
  data :: Array 0 Byte
}

gifGemini1Parser :: Parser GifGemini1
gifGemini1Parser = do
  header' <- optional (bytes 3)
  width' <- beWord32
  height' <- beWord32
  data' <- bytes 0
  return GifGemini1 { header = header', width = width', height = height', data = data' }

instance  Show GifGemini1 where
  show (GifGemini1 h w h' d) = "GifGemini1 { header = " ++ show h ++ ", width = " ++ show w ++ ", height = " ++ show h' ++ ", data = " ++ show d ++ " }"

-- Helper functions (if needed)
beWord32 :: Parser Word32
beWord32 = do
  bs <- bytes 4
  return $ runGet (getWord32be bs)


