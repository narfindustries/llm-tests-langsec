def TLS_ClientHello:
  u16 legacy_version = 0x0303
  array u8[32] random
  u8 legacy_session_id_len
  array u8[legacy_session_id_len] legacy_session_id
  u16 cipher_suites_len
  array u16[cipher_suites_len / 2] cipher_suites
  u8 legacy_compression_methods_len
  array u8[legacy_compression_methods_len] legacy_compression_methods
  u16 extensions_len
  array {
    u16 extension_type
    u16 extension_data_length
    array u8[extension_data_length] extension_data
  }[extensions_len] extensions