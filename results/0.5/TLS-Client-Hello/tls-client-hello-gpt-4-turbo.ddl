type TLSClientHello = {
  handshake_type : U8;
  length : U24;
  version : ProtocolVersion;
  random : Random;
  session_id : SessionID;
  cipher_suites : CipherSuites;
  compression_methods : CompressionMethods;
  extensions : Extensions;
}

type ProtocolVersion = {
  major : U8;
  minor : U8;
}

type Random = {
  gmt_unix_time : U32;
  random_bytes : Bytes<28>;
}

type SessionID = {
  length : U8;
  session_id : Bytes<length>;
}

type CipherSuites = {
  length : U16;
  cipher_suites : List<CipherSuite, length / 2>;
}

type CipherSuite = U16;

type CompressionMethods = {
  length : U8;
  compression_methods : List<U8, length>;
}

type Extensions = {
  length : U16;
  extensions : List<Extension, length>;
}

type Extension = {
  extension_type : U16;
  extension_data : ExtensionData;
}

type ExtensionData = {
  length : U16;
  data : Bytes<length>;
}