let TLSRecordLayer = struct {
    contentType: UInt8;
    version: UInt16;
    length: UInt16;
    handshake: HandshakeProtocol;
};

let HandshakeProtocol = struct {
    msgType: UInt8;
    length: UInt24;
    handshakeMessage: HandshakeMessage;
};

let HandshakeMessage = struct {
    clientVersion: UInt16;
    random: Bytes[32];
    sessionId: SessionId;
    cipherSuites: CipherSuites;
    compressionMethods: CompressionMethods;
    extensions: Extensions;
};

let SessionId = struct {
    length: UInt8;
    sessionId: Bytes[this.length];
};

let CipherSuites = struct {
    length: UInt16;
    cipherSuites: Bytes[this.length];
};

let CompressionMethods = struct {
    length: UInt8;
    compressionMethods: Bytes[this.length];
};

let Extensions = struct {
    length: UInt16;
    extensions: Extension[this.length / 4];
};

let Extension = struct {
    extensionType: UInt16;
    length: UInt16;
    extensionData: Bytes[this.length];
};

let UInt24 = struct {
    byte1: UInt8;
    byte2: UInt8;
    byte3: UInt8;
};

let TLSClientHello = TLSRecordLayer;