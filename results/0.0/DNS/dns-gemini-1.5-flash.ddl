{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module DNS where

import Daedalus.AST
import Daedalus.Type
import Daedalus.Value
import Daedalus.PP

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.List as List

-- | A simple DNS record type
data DNSRecord = A { ip :: BS.ByteString }
              | CNAME { name :: BS.ByteString }
              deriving (Show, Generic, ToJSON, FromJSON, ToDaedalus)

-- | A DNS message type
data DNSMessage = DNSMessage {
  header :: DNSHeader,
  questions :: [DNSQuestion],
  answers :: [DNSRecord],
  authority :: [DNSRecord],
  additional :: [DNSRecord]
} deriving (Show, Generic, ToJSON, FromJSON, ToDaedalus)

-- | DNS header
data DNSHeader = DNSHeader {
  id :: Word16,
  qr :: Bool,
  opcode :: Word8,
  aa :: Bool,
  tc :: Bool,
  rd :: Bool,
  ra :: Bool,
  z :: Word8,
  rcode :: Word8,
  qdcount :: Word16,
  ancount :: Word16,
  nscount :: Word16,
  arcount :: Word16
} deriving (Show, Generic, ToJSON, FromJSON, ToDaedalus)

-- | DNS question
data DNSQuestion = DNSQuestion {
  qname :: BS.ByteString,
  qtype :: Word16,
  qclass :: Word16
} deriving (Show, Generic, ToJSON, FromJSON, ToDaedalus)


-- | Parser for a DNS message
parseDNS :: Parser DNSMessage
parseDNS = do
  header <- parseDNSHeader
  questions <- many (parseDNSQuestion)
  answers <- many (parseDNSRecord)
  authority <- many (parseDNSRecord)
  additional <- many (parseDNSRecord)
  return $ DNSMessage header questions answers authority additional

-- | Parser for a DNS header
parseDNSHeader :: Parser DNSHeader
parseDNSHeader = do
  id' <- beWord16
  flags <- beWord16
  qdcount <- beWord16
  ancount <- beWord16
  nscount <- beWord16
  arcount <- beWord16
  let qr' = (flags `shiftR` 15) == 1
  let opcode' = (flags `shiftR` 11) .&. 0xF
  let aa' = (flags `shiftR` 10) == 1
  let tc' = (flags `shiftR` 9) == 1
  let rd' = (flags `shiftR` 8) == 1
  let ra' = (flags `shiftR` 7) == 1
  let z' = (flags `shiftR` 4) .&. 0x7
  let rcode' = flags .&. 0xF
  return $ DNSHeader id' qr' opcode' aa' tc' rd' ra' z' rcode' qdcount ancount nscount arcount

-- | Parser for a DNS question
parseDNSQuestion :: Parser DNSQuestion
parseDNSQuestion = do
  qname <- parseName
  qtype <- beWord16
  qclass <- beWord16
  return $ DNSQuestion qname qtype qclass

-- | Parser for a DNS record
parseDNSRecord :: Parser DNSRecord
parseDNSRecord = do
  rtype <- beWord16
  case rtype of
    1 -> do
      ip' <- parseIP
      return $ A ip'
    5 -> do
      cname <- parseName
      return $ CNAME cname
    _ -> fail "Unsupported record type"

-- | Parser for a domain name
parseName :: Parser BS.ByteString
parseName = do
  labels <- many parseLabel
  return $ BS.concat labels

-- | Parser for a label
parseLabel :: Parser BS.ByteString
parseLabel = do
  len <- beUInt8
  if len == 0 then return BS.empty else BS.take len <$> take len

-- | Parser for an IPv4 address
parseIP :: Parser BS.ByteString
parseIP = BS.pack <$> replicateM 4 beUInt8

-- | Helper function to read a word in big-endian order
beWord16 :: Parser Word16
beWord16 = do
  h <- beUInt8
  l <- beUInt8
  return $ fromIntegral h `shiftL` 8 .|. fromIntegral l

-- | Helper function to read an unsigned integer in big-endian order
beUInt8 :: Parser Word8
beUInt8 = getUInt8

