type TLSClientHello = {
    handshake_type: u8,
    length: u24,
    legacy_version: u16,
    random: [u8; 32],
    legacy_session_id_length: u8,
    legacy_session_id: [u8; legacy_session_id_length],
    cipher_suites_length: u16,
    cipher_suites: [u16; cipher_suites_length / 2],
    compression_methods_length: u8,
    compression_methods: [u8; compression_methods_length],
    extensions_length: u16,
    extensions: [Extension]
} invariant {
    handshake_type == 0x01,
    legacy_version == 0x0303
};

type Extension = {
    extension_type: u16,
    extension_length: u16,
    extension_data: [u8; extension_length]
};

type ServerNameExtension = Extension where {
    extension_type == 0x0000,
    list_length: u16,
    server_names: [ServerName; list_length]
};

type ServerName = {
    name_type: u8,
    name_length: u16,
    hostname: [u8; name_length]
};

type SupportedVersionsExtension = Extension where {
    extension_type == 0x002B,
    versions_length: u8,
    versions: [u16; versions_length / 2]
};

type SignatureAlgorithmsExtension = Extension where {
    extension_type == 0x000D,
    algorithms_length: u16,
    algorithms: [u16; algorithms_length / 2]
};

type SupportedGroupsExtension = Extension where {
    extension_type == 0x000A,
    groups_length: u16,
    groups: [u16; groups_length / 2]
};

type KeyShareExtension = Extension where {
    extension_type == 0x0033,
    key_share_length: u16,
    key_shares: [KeyShare; key_share_length]
};

type KeyShare = {
    group: u16,
    key_exchange_length: u16,
    key_exchange: [u8; key_exchange_length]
};

type PreSharedKeyExtension = Extension where {
    extension_type == 0x0029,
    identities_length: u16,
    identities: [PreSharedKeyIdentity; identities_length],
    binders_length: u16,
    binders: [PreSharedKeyBinder; binders_length]
};

type PreSharedKeyIdentity = {
    identity_length: u16,
    identity: [u8; identity_length],
    obfuscated_ticket_age: u32
};

type PreSharedKeyBinder = {
    binder_length: u8,
    binder: [u8; binder_length]
};

type PSKKeyExchangeModesExtension = Extension where {
    extension_type == 0x002D,
    modes_length: u8,
    modes: [u8; modes_length]
};

type ALPNExtension = Extension where {
    extension_type == 0x0010,
    protocol_names_length: u16,
    protocol_names: [ALPNProtocol; protocol_names_length]
};

type ALPNProtocol = {
    protocol_length: u8,
    protocol_name: [u8; protocol_length]
};