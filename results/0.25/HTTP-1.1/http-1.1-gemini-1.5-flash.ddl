module HTTP11 where

import Daedalus.AST
import Daedalus.Value

data HTTPMethod = GET | POST | PUT | DELETE | HEAD | OPTIONS | CONNECT | TRACE | PATCH
  deriving (Eq, Show)

data HTTPVersion = HTTP11 | HTTP10 | HTTP2
  deriving (Eq, Show)

data HTTPRequest = HTTPRequest
  { method :: HTTPMethod
  , uri :: String
  , version :: HTTPVersion
  , headers :: [(String, String)]
  , body :: ByteString
  } deriving (Show)

data HTTPResponse = HTTPResponse
  { version :: HTTPVersion
  , status :: Int
  , reason :: String
  , headers :: [(String, String)]
  , body :: ByteString
  } deriving (Show)

parseHTTPMethod :: Parser HTTPMethod
parseHTTPMethod = choice
  [ string "GET" >> return GET
  , string "POST" >> return POST
  , string "PUT" >> return PUT
  , string "DELETE" >> return DELETE
  , string "HEAD" >> return HEAD
  , string "OPTIONS" >> return OPTIONS
  , string "CONNECT" >> return CONNECT
  , string "TRACE" >> return TRACE
  , string "PATCH" >> return PATCH
  ]

parseHTTPVersion :: Parser HTTPVersion
parseHTTPVersion = choice
  [ string "HTTP/1.1" >> return HTTP11
  , string "HTTP/1.0" >> return HTTP10
  , string "HTTP/2" >> return HTTP2
  ]

parseHeaderLine :: Parser (String, String)
parseHeaderLine = do
  name <- some (satisfy isAlphaNum)
  colon <- char ':'
  value <- many (satisfy (\c -> c /= '\r'))
  crlf <- string "\r\n"
  return (name, value)

parseHeaders :: Parser [(String, String)]
parseHeaders = many parseHeaderLine

parseHTTPRequest :: Parser HTTPRequest
parseHTTPRequest = do
  method <- parseHTTPMethod
  spaces
  uri <- some (satisfy (\c -> c /= ' '))
  spaces
  version <- parseHTTPVersion
  crlf <- string "\r\n"
  headers <- parseHeaders
  crlf2 <- string "\r\n"
  body <- many anyChar
  return $ HTTPRequest { method = method, uri = uri, version = version, headers = headers, body = pack body }

parseHTTPResponse :: Parser HTTPResponse
parseHTTPResponse = do
  version <- parseHTTPVersion
  spaces
  status <- decimal
  spaces
  reason <- many (satisfy (\c -> c /= '\r'))
  crlf <- string "\r\n"
  headers <- parseHeaders
  crlf2 <- string "\r\n"
  body <- many anyChar
  return $ HTTPResponse { version = version, status = status, reason = reason, headers = headers, body = pack body }

main :: IO ()
main = do
  let requestBytes = pack "GET /index.html HTTP/1.1\r\nHost: example.com\r\n\r\n"
  case runParser parseHTTPRequest requestBytes of
    Just req -> print req
    Nothing -> putStrLn "Failed to parse request"

  let responseBytes = pack "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\n\r\n<html>...</html>"
  case runParser parseHTTPResponse responseBytes of
    Just resp -> print resp
    Nothing -> putStrLn "Failed to parse response"
