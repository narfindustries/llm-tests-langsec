meta:
  id: tls_client_hello
  endian: be
seq:
  - id: record_header
    type: record_header
  - id: handshake_header
    type: handshake_header
  - id: client_version
    type: version
  - id: random
    type: random_struct
  - id: session_id
    type: session_id_struct
  - id: cipher_suites
    type: cipher_suites_struct
  - id: compression_methods
    type: compression_methods_struct
  - id: extensions_length
    type: u2
    if: _io.pos < _io.size
  - id: extensions
    type: extension
    repeat: eos
    if: extensions_length > 0
types:
  record_header:
    seq:
      - id: content_type
        type: u1
      - id: version
        type: version
      - id: length
        type: u2
  handshake_header:
    seq:
      - id: msg_type
        type: u1
      - id: length
        type: b24
  version:
    seq:
      - id: major
        type: u1
      - id: minor
        type: u1
  random_struct:
    seq:
      - id: gmt_unix_time
        type: u4
      - id: random_bytes
        size: 28
  session_id_struct:
    seq:
      - id: length
        type: u1
      - id: session_id
        size: length
        if: length > 0
  cipher_suites_struct:
    seq:
      - id: length
        type: u2
      - id: cipher_suites
        type: u2
        repeat: expr
        repeat-expr: length / 2
  compression_methods_struct:
    seq:
      - id: length
        type: u1
      - id: compression_methods
        size: length
  extension:
    seq:
      - id: type
        type: u2
      - id: length
        type: u2
      - id: data
        size: length