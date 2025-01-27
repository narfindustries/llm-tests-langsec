type TLSClientHello = struct {
  contentType         : UInt8;         // Type of the record
  version             : UInt16;        // Protocol version
  length              : UInt16;        // Length of the following data
  handshakeType       : UInt8;         // Handshake type
  lengthHandshake     : UInt24;        // Length of the handshake
  versionHandshake    : UInt16;        // Version in handshake
  random              : bytes(32);     // Random data
  sessionIdLength     : UInt8;         // Session ID length
  sessionId           : bytes(sessionIdLength);  // Session ID
  cipherSuitesLength  : UInt16;        // Length of cipher suites
  cipherSuites        : bytes(cipherSuitesLength); // Cipher suites
  compressionMethodsLength : UInt8;    // Length of compression methods
  compressionMethods  : bytes(compressionMethodsLength); // Compression methods
  extensionsLength    : UInt16;        // Length of extensions
  extensions          : bytes(extensionsLength); // Extensions data
}
