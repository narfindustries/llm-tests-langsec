specification TLSClientHello {
    // TLS Client Hello message structure
    type ClientHello = {
        record_type: uint8,
        protocol_version: uint16,
        length: uint16,
        handshake_type: uint8,
        handshake_length: uint24,
        client_version: uint16,
        random: bytes[32],
        session_id_length: uint8,
        session_id: bytes[session_id_length],
        cipher_suites_length: uint16,
        cipher_suites: bytes[cipher_suites_length],
        compression_methods_length: uint8,
        compression_methods: bytes[compression_methods_length],
        extensions_length: uint16,
        extensions: bytes[extensions_length]
    }

    // Parsing rules for TLS Client Hello
    parse ClientHello {
        record_type = 22;  // Handshake record type
        protocol_version = 0x0303;  // TLS 1.2
        handshake_type = 1;  // Client Hello type
        client_version = 0x0303;  // TLS 1.2 client version
        random = generate_random_bytes(32);
        session_id_length = 32;
        session_id = generate_random_bytes(session_id_length);
        cipher_suites_length = 4;
        cipher_suites = [0x13, 0x02, 0x13, 0x03];  // TLS_AES_256_GCM_SHA384, TLS_CHACHA20_POLY1305_SHA256
        compression_methods_length = 1;
        compression_methods = [0x00];  // No compression
        extensions_length = 10;
        extensions = generate_random_bytes(extensions_length)
    }

    // Helper function to generate random bytes
    function generate_random_bytes(length: uint8): bytes[length] {
        // Placeholder for random byte generation
        return bytes[length] { 0x00 }
    }
}