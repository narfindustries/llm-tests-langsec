// Daedalus specification for TLS Client Hello message
namespace tls

// Define the TLS Client Hello structure
struct ClientHello {
    uint16 version; // Protocol version
    Random random; // Random structure
    SessionID session_id; // Session ID
    CipherSuites cipher_suites; // Cipher suites
    CompressionMethods compression_methods; // Compression methods
    Extensions extensions; // Extensions
}

// Random structure
struct Random {
    uint32 gmt_unix_time; // Unix time
    uint8[28] random_bytes; // Random bytes
}

// Session ID structure
struct SessionID {
    uint8 length; // Length of session ID
    uint8[length] session_id; // Session ID bytes
}

// Cipher suites structure
struct CipherSuites {
    uint16 length; // Length of cipher suites
    uint16[length / 2] suites; // Cipher suites
}

// Compression methods structure
struct CompressionMethods {
    uint8 length; // Length of compression methods
    uint8[length] methods; // Compression methods
}

// Extensions structure
struct Extensions {
    uint16 length; // Length of extensions
    Extension[length] extensions; // Extensions
}

// Extension structure
struct Extension {
    uint16 type; // Extension type
    uint16 length; // Length of extension data
    uint8[length] data; // Extension data
}