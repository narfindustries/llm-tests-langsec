data HTTPMethod = GET | POST | PUT | DELETE | HEAD | OPTIONS | CONNECT | TRACE | PATCH
  deriving (Eq, Show, Enum)

data HTTPVersion = HTTP1_1 deriving (Eq, Show)

data HTTPHeader = HTTPHeader {
  name :: String,
  value :: String
} deriving (Eq, Show)

data HTTPRequest = HTTPRequest {
  method :: HTTPMethod,
  uri :: String,
  version :: HTTPVersion,
  headers :: [HTTPHeader],
  body :: ByteString
} deriving (Eq, Show)

data HTTPResponse = HTTPResponse {
  version :: HTTPVersion,
  status :: Int,
  reason :: String,
  headers :: [HTTPHeader],
  body :: ByteString
} deriving (Eq, Show)

data HTTPMessage = HTTPRequest HTTPRequest | HTTPResponse HTTPResponse
  deriving (Eq, Show)

parseHTTPMethod :: ByteString -> Maybe HTTPMethod
parseHTTPMethod bs = case decodeUtf8 bs of
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

parseHTTPVersion :: ByteString -> Maybe HTTPVersion
parseHTTPVersion bs = case decodeUtf8 bs of
  "HTTP/1.1" -> Just HTTP1_1
  _ -> Nothing

parseHTTPHeader :: ByteString -> Maybe HTTPHeader
parseHTTPHeader bs =
  case break (== ':') (decodeUtf8 bs) of
    (name, ':':value) -> Just $ HTTPHeader (trim name) (trim value)
    _ -> Nothing

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

parseHTTPRequest :: ByteString -> Maybe HTTPRequest
parseHTTPRequest = undefined --Implementation omitted for brevity

parseHTTPResponse :: ByteString -> Maybe HTTPResponse
parseHTTPResponse = undefined --Implementation omitted for brevity

parseHTTP :: ByteString -> Maybe HTTPMessage
parseHTTP = undefined --Implementation omitted for brevity
