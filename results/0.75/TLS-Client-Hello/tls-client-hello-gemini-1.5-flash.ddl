record ClientHello {
  uint16 client_version;
  bytes random[32];
  bytes session_id;
  uint16 cipher_suites_length;
  bytes cipher_suites[cipher_suites_length];
  uint8 compression_methods_length;
  bytes compression_methods[compression_methods_length];
  optional uint16 extensions_length;
  optional bytes extensions[extensions_length];
}
