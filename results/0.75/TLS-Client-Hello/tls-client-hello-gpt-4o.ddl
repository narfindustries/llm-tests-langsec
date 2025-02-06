ClientHello {
    uint16 client_version;
    uint8 random[32];
    
    LegacySessionID {
        uint8 length;
        if (length > 0) {
            uint8 session_id[length];
        }
    } legacy_session_id;
    
    CipherSuites {
        uint16 length;
        uint16 cipher_suites[length / 2];
    } cipher_suites;
    
    CompressionMethods {
        uint8 length;
        uint8 compression_methods[length];
    } legacy_compression_methods;
    
    Extensions {
        uint16 length;
        Extension extensions[];
    } extensions;
}

Extension {
    uint16 type;
    uint16 length;
    uint8 data[length];
}