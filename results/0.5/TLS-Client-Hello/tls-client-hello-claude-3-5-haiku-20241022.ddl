type TLSClientHello = record {
  handshake_type: u8 where self == 0x01,
  length: u24,
  client_version: u16 where self == 0x0303,
  random: [u8; 32],
  legacy_session_id_length: u8,
  legacy_session_id: [u8; legacy_session_id_length],
  cipher_suites_length: u16,
  cipher_suites: [u16; cipher_suites_length / 2],
  legacy_compression_methods_length: u8,
  legacy_compression_methods: [u8; legacy_compression_methods_length],
  extensions_length: u16,
  extensions: [Extension; extensions_length]
};

type Extension = record {
  extension_type: u16,
  extension_length: u16,
  extension_data: match extension_type {
    0x0000 => ServerNameExtension,
    0x002B => SupportedVersionsExtension,
    0x000D => SignatureAlgorithmsExtension,
    0x000A => SupportedGroupsExtension,
    0x0033 => KeyShareExtension,
    0x0029 => PreSharedKeyExtension,
    0x002D => PSKKeyExchangeModesExtension,
    _ => [u8; extension_length]
  }
};

type ServerNameExtension = record {
  server_name_list_length: u16,
  server_names: [ServerName; server_name_list_length]
};

type ServerName = record {
  name_type: u8,
  name_length: u16,
  hostname: [u8; name_length]
};

type SupportedVersionsExtension = record {
  supported_versions_length: u8,
  supported_versions: [u16; supported_versions_length / 2]
};

type SignatureAlgorithmsExtension = record {
  signature_algorithms_length: u16,
  signature_algorithms: [u16; signature_algorithms_length / 2]
};

type SupportedGroupsExtension = record {
  supported_groups_length: u16,
  supported_groups: [u16; supported_groups_length / 2]
};

type KeyShareExtension = record {
  key_share_length: u16,
  key_shares: [KeyShareEntry; key_share_length]
};

type KeyShareEntry = record {
  group: u16,
  key_exchange_length: u16,
  key_exchange: [u8; key_exchange_length]
};

type PreSharedKeyExtension = record {
  identities_length: u16,
  identities: [PskIdentity; identities_length],
  binders_length: u16,
  binders: [PskBinderEntry; binders_length]
};

type PskIdentity = record {
  identity_length: u16,
  identity: [u8; identity_length],
  obfuscated_ticket_age: u32
};

type PskBinderEntry = record {
  binder_length: u8,
  binder: [u8; binder_length]
};

type PSKKeyExchangeModesExtension = record {
  ke_modes_length: u8,
  ke_modes: [u8; ke_modes_length]
};