module HTTP_1_1 where

import Network.IPv4
import Network.TCP

-- Definition of the HTTP/1.1 protocol based on RFC 2616
data HttpMethod = GET | POST | HEAD | PUT | DELETE | TRACE | OPTIONS | CONNECT | PATCH
  deriving (Enum, Show)

data HttpStatus = Continue | OK | Created | Accepted | NonAuthoritativeInformation
                | NoContent | ResetContent | PartialContent
                | MultipleChoices | MovedPermanently | Found | SeeOther
                | NotModified | UseProxy | Unused | TemporaryRedirect | PermanentRedirect
                | BadRequest | Unauthorized | PaymentRequired | Forbidden | NotFound
                | MethodNotAllowed | NotAcceptable | ProxyAuthRequired | RequestTimeout
                | Conflict | Gone | LengthRequired | PreconditionFailed
                | RequestEntityTooLarge | RequestURITooLong | UnsupportedMediaType
                | RequestedRangeNotSatisfiable | ExpectationFailed | UpgradeRequired
                | InternalServerError | NotImplemented | BadGateway | ServiceUnavailable
                | GatewayTimeout | HTTPVersionNotSupported
  deriving (Enum, Show)

data HttpVersion = HTTP_1_0 | HTTP_1_1

data RequestLine = RequestLine {
  method :: HttpMethod,
  requestURI :: String,
  httpVersion :: HttpVersion
}

data StatusLine = StatusLine {
  httpVersion :: HttpVersion,
  statusCode :: HttpStatus,
  reasonPhrase :: String
}

data Header = Header {
  fieldName :: String,
  fieldValue :: String
}

type Headers = [Header]

data MessageBody = MessageBody {
  content :: String
}

data HttpRequest = HttpRequest {
  requestLine :: RequestLine,
  headers :: Headers,
  messageBody :: Maybe MessageBody
}

data HttpResponse = HttpResponse {
  statusLine :: StatusLine,
  headers :: Headers,
  messageBody :: Maybe MessageBody
}

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

parseHttpStatus :: Parser HttpStatus
parseHttpStatus = choice [
  string "100" *> pure Continue,
  string "200" *> pure OK,
  string "201" *> pure Created,
  -- Add other statuses similarly
  string "500" *> pure InternalServerError
  ]

parseHttpVersion :: Parser HttpVersion
parseHttpVersion = choice [
  string "HTTP/1.0" *> pure HTTP_1_0,
  string "HTTP/1.1" *> pure HTTP_1_1
  ]

parseRequestLine :: Parser RequestLine
parseRequestLine = RequestLine <$> parseHttpMethod <* space <*> manyTill anyChar space <*> parseHttpVersion <* crlf

parseStatusLine :: Parser StatusLine
parseStatusLine = StatusLine <$> parseHttpVersion <* space <*> parseHttpStatus <* space <*> manyTill anyChar crlf

parseHeader :: Parser Header
parseHeader = Header <$> manyTill anyChar (char ':') <* char ' ' <*> manyTill anyChar crlf

parseHeaders :: Parser Headers
parseHeaders = many parseHeader <* crlf

parseMessageBody :: Parser MessageBody
parseMessageBody = MessageBody <$> many anyChar

parseHttpRequest :: Parser HttpRequest
parseHttpRequest = HttpRequest <$> parseRequestLine <*> parseHeaders <*> optional parseMessageBody

parseHttpResponse :: Parser HttpResponse
parseHttpResponse = HttpResponse <$> parseStatusLine <*> parseHeaders <*> optional parseMessageBody