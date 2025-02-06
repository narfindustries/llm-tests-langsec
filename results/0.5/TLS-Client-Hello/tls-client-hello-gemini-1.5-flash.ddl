type tls_client_hello = record {
    client_version: uint16;
    random: bytes[32];
    session_id: bytes?;
    cipher_suites: seq of uint16;
    compression_methods: seq of uint8;
    extensions: seq of tls_extension;
};

type tls_extension = record {
    extension_type: uint16;
    extension_data: bytes;
};

type supported_versions_extension = record {
    versions: seq of uint16;
};

type supported_groups_extension = record {
    named_groups: seq of uint16;
};

type signature_algorithms_extension = record {
    signature_algorithms: seq of uint16;
};

type key_share_extension = record {
    key_shares: seq of key_share_entry;
};

type key_share_entry = record {
    group: uint16;
    key_exchange: bytes;
};

type psk_key_exchange_modes_extension = record {
    modes: seq of uint8;
};

type session_ticket_extension = record {};

type alpn_protocols_extension = record {
    protocols: seq of bytes;
};

type server_name_extension = record {
    name_type: uint8;
    server_name: bytes;
};

type signed_certificate_timestamp_extension = record {
    sct_list: seq of bytes;
};

// Add other extensions as needed...  This is not exhaustive.
