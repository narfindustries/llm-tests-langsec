module HTTP_1_1 where

import Network.IPv4
import Data.ByteString as BS
import Data.ByteString.Char8 as C8
import Data.ByteString.Base64 as B64
import Data.DateTime

-- Define the basic structures of HTTP 1.1 based on RFC 2616

data HttpMethod = GET | POST | HEAD | PUT | DELETE | TRACE | OPTIONS | CONNECT | PATCH
  deriving (Eq, Show)

data HttpStatus = HttpStatus {
  statusCode :: UInt16,
  reasonPhrase :: String
} deriving (Eq, Show)

data HttpVersion = HTTP_1_1 | HTTP_1_0
  deriving (Eq, Show)

data HttpRequest = HttpRequest {
  method :: HttpMethod,
  uri :: String,
  httpVersion :: HttpVersion,
  headers :: [HttpHeader],
  body :: Maybe ByteString
} deriving (Eq, Show)

data HttpResponse = HttpResponse {
  version :: HttpVersion,
  status :: HttpStatus,
  responseHeaders :: [HttpHeader],
  responseBody :: Maybe ByteString
} deriving (Eq, Show)

data HttpHeader = HttpHeader {
  fieldName :: String,
  fieldValue :: String
} deriving (Eq, Show)

-- Parser definitions

parseHttpMethod :: Parser HttpMethod
parseHttpMethod = choice [
  string "GET" *> pure GET,
  string "POST" *> pure POST,
  string "HEAD" *> pure HEAD,
  string "PUT" *> pure PUT,
  string "DELETE" *> pure DELETE,
  string "TRACE" *> pure TRACE,
  string "OPTIONS" *> pure OPTIONS,
  string "CONNECT" *> pure CONNECT,
  string "PATCH" *> pure PATCH
  ]

parseHttpVersion :: Parser HttpVersion
parseHttpVersion = (string "HTTP/1.1" *> pure HTTP_1_1) <|> (string "HTTP/1.0" *> pure HTTP_1_0)

parseHttpStatus :: Parser HttpStatus
parseHttpStatus = HttpStatus <$> (decimal <* space) <*> (manyTill anyChar newline)

parseHttpHeader :: Parser HttpHeader
parseHttpHeader = HttpHeader <$> (manyTill anyChar (char ':') <* space) <*> (manyTill anyChar newline)

parseHttpRequest :: Parser HttpRequest
parseHttpRequest = HttpRequest <$>
  parseHttpMethod <* space <*>
  (manyTill anyChar space) <* space <*>
  parseHttpVersion <* newline <*>
  many parseHttpHeader <*>
  optional (newline *> many anyChar >>= return . C8.pack)

parseHttpResponse :: Parser HttpResponse
parseHttpResponse = HttpResponse <$>
  parseHttpVersion <* space <*>
  parseHttpStatus <*>
  many parseHttpHeader <*>
  optional (newline *> many anyChar >>= return . C8.pack)

-- Example usage

exampleRequest :: ByteString
exampleRequest = C8.pack "GET /index.html HTTP/1.1\r\nHost: www.example.com\r\n\r\n"

exampleResponse :: ByteString
exampleResponse = C8.pack "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\n\r\n<html></html>"

-- Parsing examples

parsedRequest :: Maybe HttpRequest
parsedRequest = parseOnly parseHttpRequest exampleRequest

parsedResponse :: Maybe HttpResponse
parsedResponse = parseOnly parseHttpResponse exampleResponse