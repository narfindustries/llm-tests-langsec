// TLS Client Hello specification for Claude 3.5 Haiku
type TLSClientHello = {
  record_type: uint8 = 0x16,  // Handshake record type
  protocol_version: uint16 = 0x0303,  // TLS 1.2
  length: uint16,
  handshake_type: uint8 = 0x01,  // Client Hello
  handshake_length: uint24,
  client_version: uint16 = 0x0303,  // TLS 1.2
  random: [uint8; 32],
  session_id_length: uint8,
  session_id: [uint8; session_id_length],
  cipher_suites_length: uint16,
  cipher_suites: [uint16; cipher_suites_length / 2],
  compression_methods_length: uint8,
  compression_methods: [uint8; compression_methods_length],
  extensions_length: uint16,
  extensions: [Extension; extensions_length]
};

type Extension = {
  extension_type: uint16,
  extension_length: uint16,
  extension_data: [uint8; extension_length]
};