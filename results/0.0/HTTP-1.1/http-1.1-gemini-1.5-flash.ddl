module HTTP11GeminiFlash where

import Daedalus.Type.AST

data HTTPMethod = GET | POST | PUT | DELETE | HEAD | OPTIONS | CONNECT | PATCH | TRACE

data HTTPVersion = HTTP11

data HTTPRequestLine = HTTPRequestLine { method :: HTTPMethod, path :: String, version :: HTTPVersion }

data HTTPHeader = HTTPHeader { name :: String, value :: String }

data HTTPRequest = HTTPRequest { requestLine :: HTTPRequestLine, headers :: [HTTPHeader], body :: String }

data GeminiRequestLine = GeminiRequestLine { geminiRequest :: String }

data GeminiResponse = GeminiResponse { geminiStatus :: Int, geminiBody :: String }

data HTTPResponse = HTTPResponse { status :: Int, headers :: [HTTPHeader], body :: String }

data FlashData = FlashData { data :: String }

data CombinedRequest = HTTPRequestData HTTPRequest | GeminiRequestData GeminiRequestLine | FlashDataData FlashData

combinedRequestParser :: Parser CombinedRequest
combinedRequestParser = choice [
    HTTPRequest <$> httpRequestParser,
    GeminiRequestData <$> geminiRequestParser,
    FlashDataData <$> flashDataParser
    ]

httpRequestParser :: Parser HTTPRequest
httpRequestParser = do
    requestLine <- httpRequestLineParser
    headers <- many httpHeaderParser
    body <- many anyChar
    return $ HTTPRequest requestLine headers (concat body)

httpRequestLineParser :: Parser HTTPRequestLine
httpRequestLineParser = do
    method <- choice [
        string "GET" >> return GET,
        string "POST" >> return POST,
        string "PUT" >> return PUT,
        string "DELETE" >> return DELETE,
        string "HEAD" >> return HEAD,
        string "OPTIONS" >> return OPTIONS,
        string "CONNECT" >> return CONNECT,
        string "PATCH" >> return PATCH,
        string "TRACE" >> return TRACE
        ]
    path <- many (satisfy isPathChar)
    version <- string "HTTP/1.1" >> return HTTP11
    return $ HTTPRequestLine method (concat path) version

httpHeaderParser :: Parser HTTPHeader
httpHeaderParser = do
    name <- many (satisfy isHeaderChar)
    colon <- char ':'
    value <- many (satisfy isHeaderChar)
    return $ HTTPHeader (concat name) (concat value)

geminiRequestParser :: Parser GeminiRequestLine
geminiRequestParser = do
    request <- many anyChar
    return $ GeminiRequestLine (concat request)

flashDataParser :: Parser FlashData
flashDataParser = do
    data <- many anyChar
    return $ FlashData (concat data)

isPathChar :: Char -> Bool
isPathChar c = isAlphaNum c || c `elem` ['/', '.', '-', '_']

isHeaderChar :: Char -> Bool
isHeaderChar c = isAlphaNum c || c `elem` [' ', '-', '_', ':', ';']

main :: Parser CombinedRequest
main = combinedRequestParser
