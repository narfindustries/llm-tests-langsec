structure TLSClientHello {
    field legacy_version: uint16;
    field random: byte[32];

    field session_id_length: uint8;
    field session_id: byte[session_id_length];

    field cipher_suites_length: uint16;
    field cipher_suites: uint16[cipher_suites_length / 2];

    field compression_methods_length: uint8;
    field compression_methods: uint8[compression_methods_length];

    field extensions_length: uint16;
    field extensions: Extension[extensions_length] {
        extensions_length > 0;
    };
}

structure Extension {
    field extension_type: uint16;
    field extension_data_length: uint16;
    field extension_data: byte[extension_data_length] {
        extension_data_length > 0;
    };
}

traits {
    ensure TLSClientHello.legacy_version == 0x0303;
}