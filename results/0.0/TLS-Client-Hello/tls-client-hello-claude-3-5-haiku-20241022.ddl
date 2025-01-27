type uint8 = bitvector(8)
type uint16 = bitvector(16)
type uint24 = bitvector(24)
type uint32 = bitvector(32)

enum ContentType {
    ChangeCipherSpec = 20,
    Alert = 21,
    Handshake = 22,
    ApplicationData = 23
}

enum HandshakeType {
    ClientHello = 1,
    ServerHello = 2,
    Certificate = 11,
    ServerKeyExchange = 12,
    ServerHelloDone = 14,
    ClientKeyExchange = 16,
    Finished = 20
}

enum ProtocolVersion {
    TLS1_0 = 0x0301,
    TLS1_1 = 0x0302,
    TLS1_2 = 0x0303,
    TLS1_3 = 0x0304
}

struct Random {
    gmt_unix_time: uint32,
    random_bytes: bitvector(28)
}

struct CipherSuite {
    first_byte: uint8,
    second_byte: uint8
}

struct CompressionMethod {
    method: uint8
}

struct ClientHello {
    protocol_version: ProtocolVersion,
    random: Random,
    session_id_length: uint8,
    session_id: bitvector(session_id_length * 8),
    cipher_suites_length: uint16,
    cipher_suites: list(CipherSuite, cipher_suites_length / 2),
    compression_methods_length: uint8,
    compression_methods: list(CompressionMethod, compression_methods_length),
    extensions_length: uint16,
    extensions: bitvector(extensions_length * 8)
}

struct HandshakeMessage {
    handshake_type: HandshakeType,
    length: uint24,
    body: match handshake_type {
        HandshakeType.ClientHello => ClientHello
    }
}

struct TLSRecord {
    content_type: ContentType,
    protocol_version: ProtocolVersion,
    length: uint16,
    fragment: match content_type {
        ContentType.Handshake => HandshakeMessage
    }
}

main = TLSRecord