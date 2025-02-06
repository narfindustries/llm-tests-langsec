format tls_client_hello {
  legacy_version: uint16 = 0x0303,
  random: bytes[32],
  legacy_session_id: bytes[0..32],
  cipher_suites: array uint16,
  legacy_compression_methods: array uint8 = [0x00],
  extensions: array extension,
}

format extension {
  extension_type: uint16,
  extension_data: bytes,
}

format server_name_extension {
  extension_type: uint16 = 0x0000,
  extension_data: server_name_extension_data,
}

format server_name_extension_data {
  server_name_list: array server_name,
}

format server_name {
  name_type: uint8,
  name: bytes,
}

format max_fragment_length_extension {
  extension_type: uint16 = 0x0001,
  extension_data: uint8,
}

format status_request_extension {
  extension_type: uint16 = 0x0005,
  extension_data: status_request_extension_data,
}

format status_request_extension_data {
  status_type: uint8,
  request: bytes,
}

format supported_groups_extension {
  extension_type: uint16 = 0x000a,
  extension_data: array uint16,
}

format signature_algorithms_extension {
  extension_type: uint16 = 0x000d,
  extension_data: array signature_algorithm,
}

format signature_algorithm {
  signature: uint8,
  algorithm: uint8,
}

format key_share_extension {
  extension_type: uint16 = 0x0033,
  extension_data: key_share_extension_data,
}

format key_share_extension_data {
  client_shares: array key_share_entry,
}

format key_share_entry {
  group: uint16,
  key_exchange: bytes,
}

format pre_shared_key_extension {
  extension_type: uint16 = 0x0029,
  extension_data: pre_shared_key_extension_data,
}

format pre_shared_key_extension_data {
  identities: array pre_shared_key_identity,
  binders: array bytes,
}

format pre_shared_key_identity {
  identity: bytes,
  obfuscated_ticket_age: uint32,
}

format early_data_extension {
  extension_type: uint16 = 0x0025,
  extension_data: uint32,
}

format supported_versions_extension {
  extension_type: uint16 = 0x002b,
  extension_data: array uint16,
}