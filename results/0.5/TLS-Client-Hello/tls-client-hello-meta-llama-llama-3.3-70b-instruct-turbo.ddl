meta {
  endian: little
}

ProtocolVersion {
  major: uint8
  minor: uint8
}

Random {
  gmt_unix_time: uint32
  random_bytes: bytes(28)
}

CipherSuite {
  value: uint16
  values {
    TLS_AES_128_GCM_SHA256: 0x1301
    TLS_CHACHA20_POLY1305_SHA256: 0x1303
    TLS_AES_256_GCM_SHA384: 0x1302
  }
}

ExtensionType {
  value: uint16
  values {
    supported_versions: 0x002b
    supported_groups: 0x000a
    key_share: 0x0033
  }
}

Extension {
  extension_type: uint16
  extension_data: bytes
}

ClientHello {
  legacy_version: ProtocolVersion
  random: Random
  legacy_session_id: bytes(0..32)
  cipher_suites: array(CipherSuite)(2..)
  legacy_compression_methods: bytes(1..255)
  extensions: array(Extension)(0..)
}