let TLSClientHello = struct {
    record: Record;
};

let Record = struct {
    type: uint8;
    version: uint16;
    length: uint16;
    handshake: Handshake;
};

let Handshake = struct {
    type: uint8;
    length: uint24;
    version: uint16;
    random: Random;
    session_id: SessionID;
    cipher_suites: CipherSuites;
    compression_methods: CompressionMethods;
    extensions: Extensions;
};

let Random = struct {
    gmt_unix_time: uint32;
    random_bytes: bytes[28];
};

let SessionID = struct {
    length: uint8;
    session_id: bytes[length];
};

let CipherSuites = struct {
    length: uint16;
    cipher_suites: bytes[length];
};

let CompressionMethods = struct {
    length: uint8;
    compression_methods: bytes[length];
};

let Extensions = struct {
    length: uint16;
    extensions: bytes[length];
};