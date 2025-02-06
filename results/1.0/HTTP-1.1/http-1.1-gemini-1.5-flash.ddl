data HTTPMethod = GET | POST | PUT | DELETE | HEAD | OPTIONS | CONNECT | TRACE | PATCH
  deriving (Eq, Show)

instance FromByteString HTTPMethod where
  fromByteString bs = case unpack bs of
    "GET" -> Just GET
    "POST" -> Just POST
    "PUT" -> Just PUT
    "DELETE" -> Just DELETE
    "HEAD" -> Just HEAD
    "OPTIONS" -> Just OPTIONS
    "CONNECT" -> Just CONNECT
    "TRACE" -> Just TRACE
    "PATCH" -> Just PATCH
    _ -> Nothing

data HTTPVersion = HTTP1_1 deriving (Eq, Show)

instance FromByteString HTTPVersion where
  fromByteString bs = case unpack bs of
    "HTTP/1.1" -> Just HTTP1_1
    _ -> Nothing

data HTTPRequest = HTTPRequest {
  method :: HTTPMethod,
  uri :: String,
  version :: HTTPVersion,
  headers :: [(String, String)],
  body :: ByteString
} deriving (Show)

data HTTPResponse = HTTPResponse {
  version :: HTTPVersion,
  status :: Int,
  reason :: String,
  headers :: [(String, String)],
  body :: ByteString
} deriving (Show)

data HTTPMessage = HTTPRequest HTTPRequest | HTTPResponse HTTPResponse
  deriving (Show)

httpHeader :: Parser (String, String)
httpHeader = do
  k <- some (satisfy isUpper) <* symbol ":"
  v <- many (satisfy (\c -> c /= '\r' && c /= '\n'))
  symbol "\r\n"
  return (k, v)

parseHTTPRequest :: Parser HTTPRequest
parseHTTPRequest = do
  method <- some (satisfy isUpper)
  symbol " "
  uri <- many (satisfy (\c -> c /= ' ' && c /= '\r'))
  symbol " "
  version <- some (satisfy isAlphaNum)
  symbol "\r\n"
  headers <- many httpHeader
  body <- option mempty (manyTill anyChar (symbol "\r\n\r\n"))
  return $ HTTPRequest {method = fromByteString (pack method), uri = uri, version = fromByteString (pack version), headers = headers, body = pack body}

parseHTTPResponse :: Parser HTTPResponse
parseHTTPResponse = do
  version <- some (satisfy isAlphaNum)
  symbol " "
  status <- decimal
  symbol " "
  reason <- many (satisfy (\c -> c /= '\r' && c /= '\n'))
  symbol "\r\n"
  headers <- many httpHeader
  body <- option mempty (manyTill anyChar eof)
  return $ HTTPResponse {version = fromByteString (pack version), status = status, reason = reason, headers = headers, body = pack body}


parseHTTP :: Parser HTTPMessage
parseHTTP = choice [
    do
      req <- parseHTTPRequest
      return $ HTTPRequest req,
    do
      resp <- parseHTTPResponse
      return $ HTTPResponse resp
    ]

main :: IO ()
main = do
  let sampleRequest = "GET / HTTP/1.1\r\nHost: example.com\r\n\r\n"
  case runParser parseHTTP (pack sampleRequest) of
    Just (HTTPRequest req, _) -> print req
    Just (HTTPResponse resp, _) -> print resp
    Nothing -> putStrLn "Failed to parse."

