// Daedalus Specification for TLS ClientHello Message
syntax = "deedles/1.19"

// TLS Protocol Version
TLSPROTOCOLVERSION = enum uint16 {
    SSL30 = 0x0300,
    TLS10 = 0x0301,
    TLS11 = 0x0302,
    TLS12 = 0x0303,
    TLS13 = 0x0304, 
}

// Cipher Suites
CIPHERSUITE = enum uint16 {
    TLS_AES_128_GCM_SHA256 = 0x1301,
    TLS_AES_256_GCM_SHA384 = 0x1302,
    TLS_CHACHA20_POLY1305_SHA256 = 0x1303,
    // Add additional cipher suites as needed
}

// Compression Methods
COMPRESSIONMETHOD = enum uint8 {
    NULL_COMPRESSION = 0x00,
    DEFLATE = 0x01,
    // Add additional compression methods if required
}

// Extensions
Extension = struct {
    uint16 extension_type;
    uint16 extension_data_length;
    bytes extension_data[extension_data_length];
}

// ClientHello Structure
ClientHello = struct {
    TLSPROTOCOLVERSION client_version;
    bytes random[32];
    uint8 session_id_length;
    bytes session_id[session_id_length];
    uint16 cipher_suites_length;
    CIPHERSUITE cipher_suites[cipher_suites_length / 2];
    uint8 compression_methods_length;
    COMPRESSIONMETHOD compression_methods[compression_methods_length];
    uint16 extensions_length;
    Extension extensions[extensions_length];
}

TLSHandshakeType = enum uint8 {
    CLIENT_HELLO = 0x01,
}

// TLS Handshake
TLSHandshake = struct {
    TLSHandshakeType handshake_type;
    uint24 length;  // 3-byte length field
    switch (handshake_type) {
        CLIENT_HELLO => ClientHello hello;
    }
}

// TLS Record Layer
TLSRecordLayer = struct {
    uint8 content_type;  // For Handshake, it's 0x16
    TLSPROTOCOLVERSION version;
    uint16 length;
    TLSHandshake handshake;
}
