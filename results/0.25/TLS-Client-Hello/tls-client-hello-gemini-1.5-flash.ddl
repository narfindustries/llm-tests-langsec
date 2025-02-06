type tls_client_hello {
  uint16 client_version;
  uint32 random_length;
  bytes random[random_length];
  uint8 legacy_session_id_length;
  bytes legacy_session_id[legacy_session_id_length];
  uint16 cipher_suites_length;
  array cipher_suites[cipher_suites_length] {
    uint16 cipher_suite;
  };
  uint8 compression_methods_length;
  array compression_methods[compression_methods_length] {
    uint8 compression_method;
  };
  uint16 extensions_length;
  array extensions[extensions_length] {
    uint16 extension_type;
    uint16 extension_data_length;
    bytes extension_data[extension_data_length];
  };
  optional uint16 early_data_length;
  optional bytes early_data[early_data_length];
}
