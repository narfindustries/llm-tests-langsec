meta:
  id: tls_client_hello
  title: TLS Client Hello Message
  endian: be

seq:
  - id: record_type
    type: u1
    enum: content_type
  
  - id: protocol_version
    type: u2
    doc: SSL/TLS protocol version

  - id: length
    type: u2
    doc: Length of the handshake message

  - id: handshake_type
    type: u1
    enum: handshake_type
    doc: Type of handshake message

  - id: handshake_length
    type: u3
    doc: Length of the handshake payload

  - id: client_version
    type: u2
    doc: TLS version supported by client

  - id: random
    size: 32
    doc: Random bytes for connection

  - id: session_id_length
    type: u1

  - id: session_id
    size: session_id_length
    doc: Session identifier

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
        enum: extension_type
      - id: length
        type: u2
      - id: body
        size: length

enums:
  content_type:
    22: handshake
    
  handshake_type:
    1: client_hello

  extension_type:
    0: server_name
    10: supported_groups
    11: ec_point_formats
    13: signature_algorithms