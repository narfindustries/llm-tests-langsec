format tls_client_hello {
  legacy_version: protocol_version,
  random: random,
  legacy_session_id: bytes,
  legacy_compression_methods: bytes,
  supported_versions: protocol_version[],
  extensions: extension[]
}

type protocol_version {
  major: uint8,
  minor: uint8
}

type random {
  gmt_unix_time: uint32,
  random_bytes: bytes
}

type extension_type {
  value: uint16,
  cases: {
    supported_groups: 10,
    key_share: 41,
    pre_shared_key: 42,
    early_data: 43,
    cookie: 44,
    supported_versions: 45,
    psk_key_exchange_modes: 46,
    ticket_early_data_info: 47,
    pre_shared_key_identities: 48,
    post_handshake_auth: 49
  }
}

type extension {
  extension_type: extension_type,
  extension_data: bytes
}