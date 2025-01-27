module TLSClientHello

import Data.ByteString
import Data.Word
import Data.List

-- TLS Client Hello message structure
data TLSClientHello = TLSClientHello
  { clientVersion :: TLSVersion
  , random        :: Random
  , sessionId     :: SessionID
  , cipherSuites  :: [CipherSuite]
  , compressionMethods :: [CompressionMethod]
  , extensions    :: [Extension]
  }

data TLSVersion = TLSVersion
  { major :: Word8
  , minor :: Word8
  }

data Random = Random
  { gmtUnixTime :: Word32
  , randomBytes :: ByteString 28
  }

data SessionID = SessionID
  { sessionIdLength :: Word8
  , sessionIdBytes  :: ByteString sessionIdLength
  }

data CipherSuite = CipherSuite
  { cipherSuite :: Word16
  }

data CompressionMethod = CompressionMethod
  { compressionMethod :: Word8
  }

data Extension = Extension
  { extensionType   :: Word16
  , extensionLength :: Word16
  , extensionData   :: ByteString extensionLength
  }

-- Helper functions
parseTLSClientHello :: ByteString -> Either String TLSClientHello
parseTLSClientHello = undefined

serializeTLSClientHello :: TLSClientHello -> ByteString
serializeTLSClientHello = undefined