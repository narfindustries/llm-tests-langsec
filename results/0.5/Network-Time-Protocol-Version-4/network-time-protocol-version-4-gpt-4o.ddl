module NetworkTimeProtocolV4 {
    li: uint2;              // Leap Indicator
    vn: uint3;              // Version Number
    mode: uint3;            // Mode
    stratum: uint8;         // Stratum
    poll: uint8;            // Poll Interval
    precision: int8;        // Precision
    rootDelay: fixed32;     // Root Delay
    rootDispersion: fixed32; // Root Dispersion
    referenceId: uint32;    // Reference Identifier
    referenceTimestamp: uint64; // Reference Timestamp
    originateTimestamp: uint64; // Originate Timestamp
    receiveTimestamp: uint64;   // Receive Timestamp
    transmitTimestamp: uint64;  // Transmit Timestamp
    
    // Optional Authenticator
    if (hasAuthenticator) {
        authenticator: Authenticator;
    }

    hasAuthenticator: bool = false; // Default to false, set to true if authenticator is present

    struct Authenticator {
        keyIdentifier: uint32; // Key Identifier
        messageDigest: bytes(16); // Message Digest (e.g., MD5, SHA-1)
    }
}