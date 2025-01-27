domain TLS {
  type HandshakeType = uint8 {
    ClientHello: 1,
    ServerHello: 2,
    Certificate: 11,
    ServerKeyExchange: 12,
    CertificateRequest: 13,
    ServerHelloDone: 14,
    CertificateVerify: 15,
    ClientKeyExchange: 16,
    Finished: 20
  }

  type ContentType = uint8 {
    ChangeCipherSpec: 20,
    Alert: 21,
    Handshake: 22,
    ApplicationData: 23
  }

  type ProtocolVersion = struct {
    major: uint8,
    minor: uint8
  }

  type Random = struct {
    gmt_unix_time: uint32,
    random_bytes: bytes[28]
  }

  type SessionID = bytes[0..32]

  type CipherSuite = uint16

  type CompressionMethod = uint8

  type ExtensionType = uint16

  type TLSPlaintext = struct {
    type: ContentType,
    version: ProtocolVersion,
    length: uint16,
    fragment: bytes[length]
  }

  type Handshake = struct {
    msg_type: HandshakeType,
    length: uint24,
    body: bytes[length]
  }

  type ClientHello = struct {
    client_version: ProtocolVersion,
    random: Random,
    session_id: SessionID,
    cipher_suites: array[CipherSuite],
    compression_methods: array[CompressionMethod],
    extensions: array[Extension]
  }

  type Extension = struct {
    type: ExtensionType,
    length: uint16,
    data: bytes[length]
  }

  grammar TLSPlaintext {
    TLSPlaintext: 
      ContentType(type) 
      ProtocolVersion(version) 
      uint16(length) 
      bytes[length](fragment)
  }

  grammar Handshake {
    Handshake: 
      HandshakeType(msg_type) 
      uint24(length) 
      bytes[length](body)
  }

  grammar ClientHello {
    ClientHello: 
      ProtocolVersion(client_version) 
      Random(random) 
      SessionID(session_id) 
      array[CipherSuite](cipher_suites) 
      array[CompressionMethod](compression_methods) 
      array[Extension](extensions)
  }

  grammar Extension {
    Extension: 
      ExtensionType(type) 
      uint16(length) 
      bytes[length](data)
  }

  grammar ProtocolVersion {
    ProtocolVersion: 
      uint8(major) 
      uint8(minor)
  }

  grammar Random {
    Random: 
      uint32(gmt_unix_time) 
      bytes[28](random_bytes)
  }

  grammar SessionID {
    SessionID: 
      bytes[0..32]
  }

  grammar CipherSuite {
    CipherSuite: 
      uint16
  }

  grammar CompressionMethod {
    CompressionMethod: 
      uint8
  }
}