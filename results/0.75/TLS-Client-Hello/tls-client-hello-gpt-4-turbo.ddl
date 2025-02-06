let ProtocolVersion = U16be

let Random = Bytes : 32

struct SessionID {
  length: U8
  session_id: Bytes : length
}

struct CipherSuites {
  length: U16be
  cipher_suites: [U16be] : length / 2
}

struct CompressionMethods {
  length: U8
  methods: [U8] : length
}

struct Extension {
  extension_type: U16be
  extension_data_length: U16be
  extension_data: Bytes : extension_data_length
}

struct Extensions {
  length: U16be
  extensions: [Extension] : length
}

struct ClientHello {
  legacy_version: ProtocolVersion
  random: Random
  session_id: SessionID
  cipher_suites: CipherSuites
  compression_methods: CompressionMethods
  extensions: Extensions
}

let start = ClientHello