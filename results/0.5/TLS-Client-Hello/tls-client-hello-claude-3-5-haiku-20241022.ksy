meta:
  id: tls_client_hello
  title: TLS Client Hello Message
  endian: be

seq:
  - id: record_type
    type: u1
    doc: TLS record type (22 for Handshake)

  - id: protocol_version
    type: u2
    doc: TLS protocol version (0x0303 for TLS 1.2, 0x0304 for TLS 1.3)

  - id: length
    type: u2
    doc: Total length of the handshake message

  - id: handshake_type
    type: u1
    doc: Handshake message type (1 for ClientHello)

  - id: handshake_length
    type: u3
    doc: Length of the ClientHello message

  - id: client_version
    type: u2
    doc: Client-proposed TLS version

  - id: random
    size: 32
    doc: Random bytes for cryptographic purposes

  - id: session_id_length
    type: u1

  - id: session_id
    size: session_id_length
    doc: Session ID for potential session resumption

  - id: cipher_suites_length
    type: u2

  - id: cipher_suites
    type: u2
    repeat: expr
    repeat-expr: cipher_suites_length / 2
    doc: Supported cipher suites

  - id: compression_methods_length
    type: u1

  - id: compression_methods
    type: u1
    repeat: expr
    repeat-expr: compression_methods_length
    doc: Supported compression methods

  - id: extensions_length
    type: u2

  - id: extensions
    type: extension
    repeat: expr
    repeat-expr: extensions_length > 0 ? 1 : 0
    doc: TLS extensions

types:
  extension:
    seq:
      - id: type
        type: u2
        doc: Extension type

      - id: length
        type: u2
        doc: Extension length

      - id: data
        size: length
        doc: Extension-specific data