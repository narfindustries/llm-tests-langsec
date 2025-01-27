module HTTP11GeminiFlash where

import Daedalus.TH

{-# LANGUAGE OverloadedStrings #-}

data HTTPMethod = GET | POST | PUT | DELETE | HEAD | OPTIONS | CONNECT | TRACE | PATCH deriving (Show, Eq, Enum, Bounded)

data HTTPVersion = HTTP11 deriving (Show, Eq)

data HTTPRequestLine = HTTPRequestLine
  { method :: HTTPMethod
  , path :: String
  , version :: HTTPVersion
  } deriving (Show, Eq)

data HTTPHeader = HTTPHeader
  { name :: String
  , value :: String
  } deriving (Show, Eq)

data HTTPRequest = HTTPRequest
  { requestLine :: HTTPRequestLine
  , headers :: [HTTPHeader]
  , body :: String
  } deriving (Show, Eq)

data GeminiRequest = GeminiRequest
  { geminiUrl :: String
  } deriving (Show, Eq)


data HTTPResponse = HTTPResponse
  { status :: Int
  , headers :: [HTTPHeader]
  , body :: String
  } deriving (Show, Eq)

data GeminiResponse = GeminiResponse
  { status :: Int
  , body :: String
  } deriving (Show, Eq)


data CombinedRequest = HTTPRequest' HTTPRequest | GeminiRequest' GeminiRequest deriving (Show, Eq)

data CombinedResponse = HTTPResponse' HTTPResponse | GeminiResponse' GeminiResponse deriving (Show, Eq)


instance Show HTTPRequestLine where
  show (HTTPRequestLine method path version) = show method ++ " " ++ path ++ " " ++ show version

instance Show HTTPRequest where
  show (HTTPRequest reqLine headers body) = show reqLine ++ "\r\n" ++ concatMap (\(HTTPHeader n v) -> n ++ ": " ++ v ++ "\r\n") headers ++ "\r\n" ++ body

instance Show HTTPResponse where
  show (HTTPResponse status headers body) = show status ++ "\r\n" ++ concatMap (\(HTTPHeader n v) -> n ++ ": " ++ v ++ "\r\n") headers ++ "\r\n" ++ body

instance Show CombinedRequest where
    show (HTTPRequest' r) = "HTTP: " ++ show r
    show (GeminiRequest' r) = "Gemini: " ++ show r

instance Show CombinedResponse where
    show (HTTPResponse' r) = "HTTP: " ++ show r
    show (GeminiResponse' r) = "Gemini: " ++ show r


$(daedalusTypeSig ''HTTPMethod)
$(daedalusTypeSig ''HTTPVersion)
$(daedalusTypeSig ''HTTPRequestLine)
$(daedalusTypeSig ''HTTPHeader)
$(daedalusTypeSig ''HTTPRequest)
$(daedalusTypeSig ''GeminiRequest)
$(daedalusTypeSig ''HTTPResponse)
$(daedalusTypeSig ''GeminiResponse)
$(daedalusTypeSig ''CombinedRequest)
$(daedalusTypeSig ''CombinedResponse)

