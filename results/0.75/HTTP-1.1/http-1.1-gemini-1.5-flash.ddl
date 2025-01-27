module HTTP-1.1-Gemini-1.5-Flash where

import Daedalus.Panic

data Request = Request { method :: String
                       , path :: String
                       , version :: String
                       , headers :: [(String, String)]
                       , body :: Maybe ByteString
                       }

data Response = Response { status :: Int
                         , reason :: String
                         , version :: String
                         , headers :: [(String, String)]
                         , body :: Maybe ByteString
                         }


parseRequest :: Parser Request
parseRequest = do
  method <- some (satisfy isUpper)
  string " "
  path <- some (satisfy (/=' '))
  string " HTTP/"
  version <- some (satisfy isDigit)
  string "\r\n"
  headers <- many headerParser
  string "\r\n\r\n"
  body <- optional (many anyChar)
  return $ Request method path version headers (Just $ pack (concat body))

parseResponse :: Parser Response
parseResponse = do
  string "HTTP/"
  version <- some (satisfy isDigit)
  string " "
  status <- decimal
  string " "
  reason <- some (satisfy (/='\r'))
  string "\r\n"
  headers <- many headerParser
  string "\r\n\r\n"
  body <- optional (many anyChar)
  return $ Response status reason version headers (Just $ pack (concat body))


headerParser :: Parser (String, String)
headerParser = do
  name <- some (satisfy (/=':'))
  char ':'
  value <- some (satisfy (/='\r'))
  string "\r\n"
  return (name, value)

main :: Parser ()
main = do
  req <- parseRequest
  -- Process request here
  pure ()

