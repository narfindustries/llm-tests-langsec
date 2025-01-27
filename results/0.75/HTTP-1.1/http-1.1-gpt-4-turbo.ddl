module HTTP_1_1 where

import DAEDALUS.Core

-- Define the structure of an HTTP message which includes
-- both the request and response format as per HTTP/1.1 specification

-- Define basic building blocks
token           = many1 (alpha <|> digit <|> oneOf "-!#$%&'*+.^_`|~")
newline         = string "\r\n"
space           = string " "

-- Request-Line = Method SP Request-URI SP HTTP-Version CRLF
requestLine     = RequestLine <$> method <*> (space *> requestURI) <*> (space *> httpVersion <* newline)

-- Method definitions (common methods included)
method          = string "GET" <|> string "POST" <|> string "HEAD" <|> string "PUT"
              <|> string "DELETE" <|> string "TRACE" <|> string "OPTIONS" <|> string "CONNECT" <|> string "PATCH"

-- Request-URI (A simplified version)
requestURI      = token

-- HTTP Version
httpVersion     = string "HTTP/" *> ((,) <$> digit <*> (char '.' *> digit))

-- Header fields: field-name ":" [ field-value ]
header          = Header <$> (token <* string ": ") <*> (manyTill anyChar newline)

-- An HTTP message either a request or a response
httpMessage     = HttpMessage <$> requestLine <*> many header <*> optional body

-- Body of the message (not parsed in detail here for simplicity)
body            = many anyChar

-- Parsing the full HTTP message
parseHttpMessage = parse httpMessage

-- Types to represent the structure
data RequestLine = RequestLine String String (Int, Int)
data Header = Header String String
data HttpMessage = HttpMessage RequestLine [Header] (Maybe String)