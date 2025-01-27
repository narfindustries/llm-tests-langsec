{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module DNS where

import Daedalus.Core
import Daedalus.AST
import Daedalus.PP

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Data.Word (Word8, Word16, Word32)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON, defaultOptions, genericToJSON, genericParseJSON, Options(fieldLabelModifier))

-- | Represents a DNS record type
data Type = A | AAAA | CNAME | MX | NS | TXT deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Represents a DNS question
data Question = Question
  { qname  :: [C8.ByteString]
  , qtype  :: Type
  , qclass :: Word16 -- always 1 (IN)
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Represents a DNS answer
data Answer = Answer
  { name  :: [C8.ByteString]
  , type' :: Type
  , class' :: Word16
  , ttl   :: Word32
  , rdata :: RData
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Represents the Resource Data field of a DNS answer
data RData = A_Record Word32
           | AAAA_Record BS.ByteString
           | CNAME_Record [C8.ByteString]
           | MX_Record Word16 [C8.ByteString]
           | NS_Record [C8.ByteString]
           | TXT_Record [C8.ByteString]
           deriving (Show, Eq, Generic, ToJSON, FromJSON)


-- | Parses a domain name (sequence of labels)
domainName :: Daedalus.Core.Parser C8.ByteString ([C8.ByteString])
domainName = many label

-- | Parses a single label in a domain name
label :: Daedalus.Core.Parser C8.ByteString C8.ByteString
label = do
  len <- integer 8
  C8.take len <$> bytes len

-- | Parses a DNS question
question :: Daedalus.Core.Parser C8.ByteString Question
question = do
  qname <- domainName
  qtype <- choice [pure A, pure AAAA, pure CNAME, pure MX, pure NS, pure TXT]
  qclass <- integer 16
  return $ Question qname qtype qclass


-- | Parses a DNS answer
answer :: Daedalus.Core.Parser C8.ByteString Answer
answer = do
  name <- domainName
  type' <- choice [pure A, pure AAAA, pure CNAME, pure MX, pure NS, pure TXT]
  class' <- integer 16
  ttl <- integer 32
  rdata <- rdataParser type'
  return $ Answer name type' class' ttl rdata


-- | Parses the Resource Data field based on the record type
rdataParser :: Type -> Daedalus.Core.Parser C8.ByteString RData
rdataParser A     = A_Record <$> integer 32
rdataParser AAAA  = AAAA_Record <$> bytes 16
rdataParser CNAME = CNAME_Record <$> domainName
rdataParser MX    = MX_Record <$> (integer 16) <*> domainName
rdataParser NS    = NS_Record <$> domainName
rdataParser TXT   = TXT_Record <$> many label
rdataParser _     = fail "Unsupported record type"


-- | Parses a DNS header (simplified)
header :: Daedalus.Core.Parser C8.ByteString ()
header = do
  void (bytes 12) -- Skip header for simplicity

-- | Parses a DNS message (simplified)
dnsMessage :: Daedalus.Core.Parser C8.ByteString ([Question], [Answer])
dnsMessage = do
  header
  qcount <- integer 16
  let questions = replicateM qcount question
  ancount <- integer 16
  let answers = replicateM ancount answer
  (,) <$> questions <*> answers

-- | Top level parser
main :: Daedalus.Core.Parser C8.ByteString ([Question], [Answer])
main = dnsMessage

instance ToJSON Question where
  toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = camelTo2 }

instance FromJSON Question where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = camelTo2 }

instance ToJSON Answer where
  toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = camelTo2 }

instance FromJSON Answer where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = camelTo2 }

instance ToJSON RData where
  toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = camelTo2 }

instance FromJSON RData where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = camelTo2 }


camelTo2 :: String -> String
camelTo2 s = map (\c -> if c `elem` ['A'..'Z'] then toLower c else c) s
