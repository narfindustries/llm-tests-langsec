def Main = ClientHello

def ClientHello = {
  $$ = {
    version = ProtocolVersion;
    random = Random;
    session_id = SessionID;
    cipher_suites = CipherSuites;
    compression_methods = CompressionMethods;
    extensions = Extensions
  }
}

def ProtocolVersion = {
  major = UInt8;
  minor = UInt8
}

def Random = {
  gmt_unix_time = UInt32;
  random_bytes = Take 28 UInt8
}

def SessionID = {
  length = UInt8;
  session_id = Take length UInt8
}

def CipherSuites = {
  length = UInt16;
  cipher_suites = Take (length / 2) UInt16
}

def CompressionMethods = {
  length = UInt8;
  compression_methods = Take length UInt8
}

def Extensions = {
  length = UInt16;
  extensions = Many Extension until END
}

def Extension = {
  type = UInt16;
  length = UInt16;
  data = Take length UInt8
}