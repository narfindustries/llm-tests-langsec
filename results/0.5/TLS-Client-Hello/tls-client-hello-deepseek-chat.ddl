let TLSClientHello = struct {
    record_type: uint8;
    protocol_version: struct {
        major: uint8;
        minor: uint8;
    };
    length: uint16;
    handshake_type: uint8;
    handshake_length: uint24;
    protocol_version_handshake: struct {
        major: uint8;
        minor: uint8;
    };
    random: bytes[32];
    session_id_length: uint8;
    session_id: bytes[session_id_length];
    cipher_suites_length: uint16;
    cipher_suites: bytes[cipher_suites_length];
    compression_methods_length: uint8;
    compression_methods: bytes[compression_methods_length];
    extensions_length: uint16;
    extensions: bytes[extensions_length];
};