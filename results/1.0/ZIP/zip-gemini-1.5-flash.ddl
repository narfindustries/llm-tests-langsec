module ZipGemini where

import Daedalus.String
import Daedalus.Map
import Daedalus.Array

data GeminiHeader = GeminiHeader {
  magic :: ByteString 4,
  version :: Word32,
  data_len :: Word32
}

parseGeminiHeader :: Parser GeminiHeader
parseGeminiHeader = do
  magic' <- bytes 4
  version' <- word32be
  data_len' <- word32be
  guard (magic' == "GEMI")
  return GeminiHeader { magic = magic', version = version', data_len = data_len' }

data GeminiData = GeminiData {
  data :: ByteString
}

parseGeminiData :: Word32 -> Parser GeminiData
parseGeminiData len = do
  data' <- bytes (fromIntegral len)
  return GeminiData { data = data' }

parseZipGemini :: Parser (Maybe GeminiHeader, Maybe GeminiData)
parseZipGemini = do
  header <- option Nothing (Just <$> parseGeminiHeader)
  case header of
    Just h -> do
      data' <- option Nothing (Just <$> parseGeminiData (data_len h))
      return (header, data')
    Nothing -> return (Nothing, Nothing)

-- Example usage (replace with your actual input)
main :: Parser ()
main = do
  result <- parseZipGemini
  case result of
    (Just header, Just data) -> do
      -- Process the header and data here.  Example:
      -- putStrLn ("Magic: " ++ show (magic header))
      -- putStrLn ("Version: " ++ show (version header))
      -- putStrLn ("Data Length: " ++ show (data_len header))
      -- putStrLn ("Data: " ++ show (data data))
      return ()
    _ -> return ()

