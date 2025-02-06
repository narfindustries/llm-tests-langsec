PDU TLSClientHello {
    uint16 client_version;
    opaque random[32];
    uint8 session_id_length;
    opaque session_id[session_id_length];
    uint16 cipher_suites_length;
    uint16 cipher_suites[cipher_suites_length / 2];
    uint8 compression_methods_length;
    uint8 compression_methods[compression_methods_length];
    uint16 extensions_length;
    Extension extensions[extensions_length] {
        uint16 extension_type;
        uint16 extension_data_length;
        opaque extension_data[extension_data_length];
    }
}