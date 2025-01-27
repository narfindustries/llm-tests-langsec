specification TLSClientHello {
    // TLS Client Hello message structure
    type ClientHello = {
        record_type: uint8,
        protocol_version: uint16,
        length: uint16,
        handshake_type: uint8,
        handshake_length: uint24,
        client_version: uint16,
        random: [uint8; 32],
        session_id_length: uint8,
        session_id: [uint8; session_id_length],
        cipher_suites_length: uint16,
        cipher_suites: [uint16; cipher_suites_length / 2],
        compression_methods_length: uint8,
        compression_methods: [uint8; compression_methods_length],
        extensions_length: uint16,
        extensions: [Extension; extensions_length]
    }

    type Extension = {
        extension_type: uint16,
        extension_length: uint16,
        extension_data: [uint8; extension_length]
    }

    // Main parsing function
    parse_client_hello(input: [uint8]) -> ClientHello {
        let client_hello = parse(input, ClientHello);
        return client_hello;
    }

    // Validation constraints
    constraints ClientHello {
        record_type == 22;  // Handshake record type
        protocol_version == 0x0303;  // TLS 1.2
        handshake_type == 1;  // Client Hello type
        client_version == 0x0303;  // TLS 1.2 client version
        session_id_length <= 32;
        compression_methods_length > 0;
        extensions_length > 0;
    }
}