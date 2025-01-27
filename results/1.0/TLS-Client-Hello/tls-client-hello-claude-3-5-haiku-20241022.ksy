meta:
  id: tls_client_hello
  title: TLS Client Hello Packet
  endian: be

seq:
  - id: record_type
    type: u1
    enum: content_type
  - id: protocol_version
    type: u2
  - id: length
    type: u2
  - id: handshake_type
    type: u1
    enum: handshake_type
  - id: handshake_length
    type: u3
  - id: client_version
    type: u2
  - id: random
    size: 32
  - id: session_id_length
    type: u1
  - id: session_id
    size: session_id_length
  - id: cipher_suites_length
    type: u2
  - id: cipher_suites
    type: u2
    repeat: expr
    repeat-expr: cipher_suites_length / 2
  - id: compression_methods_length
    type: u1
  - id: compression_methods
    type: u1
    repeat: expr
    repeat-expr: compression_methods_length
  - id: extensions_length
    type: u2
  - id: extensions
    type: extension
    repeat: expr
    repeat-expr: extensions_length > 0 ? 1 : 0

types:
  extension:
    seq:
      - id: type
        type: u2
      - id: length
        type: u2
      - id: data
        size: length

enums:
  content_type:
    22: handshake
  handshake_type:
    1: client_hello