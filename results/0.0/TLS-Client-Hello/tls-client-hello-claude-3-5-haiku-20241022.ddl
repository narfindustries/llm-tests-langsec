type TLSClientHello = {
  handshake_type : uint8;
  length : uint24;
  legacy_version : uint16;
  random : [uint8; 32];
  session_id_length : uint8;
  session_id : [uint8; session_id_length];
  cipher_suites_length : uint16;
  cipher_suites : [uint16; cipher_suites_length / 2];
  compression_methods_length : uint8;
  compression_methods : [uint8; compression_methods_length];
  extensions_length : uint16;
  extensions : [Extension; extensions_length / 4]
};

type Extension = {
  extension_type : uint16;
  extension_length : uint16;
  extension_data : [uint8; extension_length]
}