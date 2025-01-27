module HTTP_1_1 where

import Network.IPv4
import Network.TCP

data HttpMethod = GET | POST | PUT | DELETE | HEAD | OPTIONS | TRACE | CONNECT
  deriving (Eq, Show)

data HttpStatus = Continue | OK | Created | Accepted | NoContent | MovedPermanently | BadRequest | Unauthorized | Forbidden | NotFound | MethodNotAllowed | InternalServerError | NotImplemented | BadGateway | ServiceUnavailable
  deriving (Eq, Show)

data HttpVersion = HTTP_1_0 | HTTP_1_1
  deriving (Eq, Show)

data HttpRequest = HttpRequest {
  method :: HttpMethod,
  uri :: String,
  version :: HttpVersion,
  headers :: [(String, String)],
  body :: Maybe String
} deriving (Eq, Show)

data HttpResponse = HttpResponse {
  status :: HttpStatus,
  version :: HttpVersion,
  headers :: [(String, String)],
  body :: Maybe String
} deriving (Eq, Show)

parseHttpMethod :: Parser HttpMethod
parseHttpMethod = choice [
  string "GET" *> pure GET,
  string "POST" *> pure POST,
  string "PUT" *> pure PUT,
  string "DELETE" *> pure DELETE,
  string "HEAD" *> pure HEAD,
  string "OPTIONS" *> pure OPTIONS,
  string "TRACE" *> pure TRACE,
  string "CONNECT" *> pure CONNECT
  ]

parseHttpStatus :: Parser HttpStatus
parseHttpStatus = choice [
  string "100" *> pure Continue,
  string "200" *> pure OK,
  string "201" *> pure Created,
  string "202" *> pure Accepted,
  string "204" *> pure NoContent,
  string "301" *> pure MovedPermanently,
  string "400" *> pure BadRequest,
  string "401" *> pure Unauthorized,
  string "403" *> pure Forbidden,
  string "404" *> pure NotFound,
  string "405" *> pure MethodNotAllowed,
  string "500" *> pure InternalServerError,
  string "501" *> pure NotImplemented,
  string "502" *> pure BadGateway,
  string "503" *> pure ServiceUnavailable
  ]

parseHttpVersion :: Parser HttpVersion
parseHttpVersion = choice [
  string "HTTP/1.0" *> pure HTTP_1_0,
  string "HTTP/1.1" *> pure HTTP_1_1
  ]

parseHeader :: Parser (String, String)
parseHeader = do
  key <- manyTill anyChar (char ':')
  space
  value <- manyTill anyChar eol
  return (key, value)

parseHeaders :: Parser [(String, String)]
parseHeaders = many parseHeader

parseHttpRequest :: Parser HttpRequest
parseHttpRequest = do
  method <- parseHttpMethod
  space
  uri <- manyTill anyChar space
  space
  version <- parseHttpVersion
  eol
  headers <- parseHeaders
  eol
  body <- optional (many anyChar)
  return HttpRequest { method = method, uri = uri, version = version, headers = headers, body = body }

parseHttpResponse :: Parser HttpResponse
parseHttpResponse = do
  version <- parseHttpVersion
  space
  status <- parseHttpStatus
  eol
  headers <- parseHeaders
  eol
  body <- optional (many anyChar)
  return HttpResponse { status = status, version = version, headers = headers, body = body }