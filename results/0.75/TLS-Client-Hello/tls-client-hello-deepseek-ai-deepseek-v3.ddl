client_hello = {
  legacy_version: uint16,
  random: bytes(32),
  legacy_session_id: bytes(0..32),
  cipher_suites: vector<uint16, 0..65535>,
  legacy_compression_methods: vector<uint8, 1>,
  extensions: vector<extension, 0..65535>,
  legacy_session_ticket: optional<bytes(0..65535)>
}

extension = {
  extension_type: uint16,
  extension_data: bytes(0..65535)
}