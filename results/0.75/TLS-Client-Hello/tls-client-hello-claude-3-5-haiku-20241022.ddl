format TLSClientHello:
    record_type: uint8 = 22  // Handshake record type
    version: uint16 = 0x0303  // TLS 1.2
    length: uint16
    handshake_type: uint8 = 1  // ClientHello
    handshake_length: uint24
    protocol_version: uint16 = 0x0303  // TLS 1.2
    random: [32]uint8
    session_id_length: uint8
    session_id: [session_id_length]uint8
    cipher_suites_length: uint16
    cipher_suites: [cipher_suites_length/2]uint16
    compression_methods_length: uint8
    compression_methods: [compression_methods_length]uint8
    extensions_length: uint16
    extensions: [extensions_length]uint8