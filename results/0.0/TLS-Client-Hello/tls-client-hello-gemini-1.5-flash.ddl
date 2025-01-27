module TLS-Client-Hello (..) where

import Daedalus.Panic (panic)

-- Data types for TLS Client Hello messages

data ClientHello = ClientHello
  { clientVersion :: Version,
    random :: Random,
    sessionID :: Maybe SessionID,
    cipherSuites :: [CipherSuite],
    compressionMethods :: [CompressionMethod],
    extensions :: [Extension]
  }

data Version = Version {major :: Word8, minor :: Word8}

data Random = Random {randomBytes :: ByteString}

data SessionID = SessionID {sessionIDBytes :: ByteString}

data CipherSuite = CipherSuite {cipherSuiteValue :: Word16}

data CompressionMethod = CompressionMethod {compressionMethodValue :: Word8}

data Extension = Extension {extensionType :: Word16, extensionData :: ByteString}

-- Helper functions for encoding and decoding

encodeVersion :: Version -> ByteString
encodeVersion (Version major minor) = encodeWord8 major <> encodeWord8 minor

encodeRandom :: Random -> ByteString
encodeRandom (Random randomBytes) = randomBytes

encodeSessionID :: Maybe SessionID -> ByteString
encodeSessionID Nothing = ""
encodeSessionID (Just (SessionID sessionIDBytes)) = sessionIDBytes

encodeCipherSuites :: [CipherSuite] -> ByteString
encodeCipherSuites suites = mconcat (map encodeCipherSuite suites)

encodeCipherSuite :: CipherSuite -> ByteString
encodeCipherSuite (CipherSuite cipherSuiteValue) = encodeWord16BE cipherSuiteValue

encodeCompressionMethods :: [CompressionMethod] -> ByteString
encodeCompressionMethods methods = mconcat (map encodeCompressionMethod methods)

encodeCompressionMethod :: CompressionMethod -> ByteString
encodeCompressionMethod (CompressionMethod compressionMethodValue) = encodeWord8 compressionMethodValue

encodeExtensions :: [Extension] -> ByteString
encodeExtensions extensions = mconcat (map encodeExtension extensions)

encodeExtension :: Extension -> ByteString
encodeExtension (Extension extensionType extensionData) = encodeWord16BE extensionType <> encodeLength extensionData <> extensionData

encodeLength :: ByteString -> ByteString
encodeLength bs = encodeWord16BE (length bs)

encodeWord8 :: Word8 -> ByteString
encodeWord8 = packWord8

encodeWord16BE :: Word16 -> ByteString
encodeWord16BE = packWord16BE

-- Main function to encode the ClientHello message

encodeClientHello :: ClientHello -> ByteString
encodeClientHello clientHello =
  encodeWord8 0x16 -- Handshake type
  <> encodeWord24BE (length clientHelloBytes) -- Length
  <> clientHelloBytes
  where
    clientHelloBytes =
      encodeVersion (clientVersion clientHello)
      <> encodeRandom (random clientHello)
      <> encodeSessionID (sessionID clientHello)
      <> encodeCipherSuites (cipherSuites clientHello)
      <> encodeCompressionMethods (compressionMethods clientHello)
      <> encodeExtensions (extensions clientHello)


-- Example usage

main :: IO ()
main = do
  let clientHello =
        ClientHello
          { clientVersion = Version 3 1,
            random = Random "random bytes",
            sessionID = Nothing,
            cipherSuites = [],
            compressionMethods = [],
            extensions = []
          }
  let encoded = encodeClientHello clientHello
  putStrLn $ "Encoded Client Hello: " <> show encoded

