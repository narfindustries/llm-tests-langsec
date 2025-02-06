def tls_client_hello = record {
    handshake_type: uint8;
    length: uint24;
    legacy_version: uint16;
    random: [32]uint8;
    session_id_length: uint8;
    session_id: if session_id_length > 0 then [session_id_length]uint8 else [];
    cipher_suites_length: uint16;
    cipher_suites: [cipher_suites_length / 2]uint16;
    compression_methods_length: uint8;
    compression_methods: [compression_methods_length]uint8;
    extensions_length: uint16;
    extensions: [extensions_length]extension
};

def extension = record {
    extension_type: uint16;
    extension_length: uint16;
    extension_data: match extension_type {
        0x0000 => server_name_ext,
        0x000A => supported_groups_ext,
        0x000D => signature_algorithms_ext,
        0x0010 => alpn_ext,
        0x0029 => pre_shared_key_ext,
        0x002B => supported_versions_ext,
        0x002D => psk_key_exchange_modes_ext,
        0x0033 => key_share_ext,
        _ => opaque_ext
    }
};

def server_name_ext = record {
    server_name_list_length: uint16;
    server_names: [server_name_list_length]server_name
};

def server_name = record {
    name_type: uint8;
    name_length: uint16;
    hostname: [name_length]uint8
};

def supported_groups_ext = record {
    groups_length: uint16;
    groups: [groups_length / 2]uint16
};

def signature_algorithms_ext = record {
    algorithms_length: uint16;
    algorithms: [algorithms_length / 2]uint16
};

def alpn_ext = record {
    alpn_length: uint16;
    protocol_names: [alpn_length]alpn_protocol
};

def alpn_protocol = record {
    protocol_length: uint8;
    protocol_name: [protocol_length]uint8
};

def pre_shared_key_ext = record {
    identities_length: uint16;
    identities: [identities_length]psk_identity;
    binders_length: uint16;
    binders: [binders_length]psk_binder
};

def psk_identity = record {
    identity_length: uint16;
    identity: [identity_length]uint8;
    obfuscated_ticket_age: uint32
};

def psk_binder = record {
    binder_length: uint8;
    binder: [binder_length]uint8
};

def supported_versions_ext = record {
    versions_length: uint8;
    versions: [versions_length / 2]uint16
};

def psk_key_exchange_modes_ext = record {
    modes_length: uint8;
    modes: [modes_length]uint8
};

def key_share_ext = record {
    client_shares_length: uint16;
    client_shares: [client_shares_length]key_share_entry
};

def key_share_entry = record {
    group: uint16;
    key_exchange_length: uint16;
    key_exchange: [key_exchange_length]uint8
};

def opaque_ext = record {
    opaque_data: [_]uint8
};