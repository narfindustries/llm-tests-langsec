module TLS_ClientHello;

struct ProtocolVersion {
  u16 value;
}

struct Random {
  u8 bytes[32];
}

struct SessionID {
  u8 length;
  u8 session_id[length];
}

struct CipherSuites {
  u16 length;
  u16 cipher_suites[length / 2];
}

struct CompressionMethods {
  u8 length;
  u8 methods[length];
}

struct Extension {
  u16 extension_type;
  u16 extension_data_length;
  u8 extension_data[extension_data_length];
}

struct Extensions {
  u16 length;
  Extension extensions[length];
}

struct ClientHello {
  ProtocolVersion legacy_version;
  Random random;
  SessionID legacy_session_id;
  CipherSuites cipher_suites;
  CompressionMethods legacy_compression_methods;
  Extensions extensions;
}

struct TLSPlaintext {
  u8 type;
  ProtocolVersion version;
  u16 length;
  ClientHello fragment;
}