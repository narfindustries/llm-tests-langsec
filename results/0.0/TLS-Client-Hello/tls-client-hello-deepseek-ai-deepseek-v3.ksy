meta:
  id: tls_client_hello
  file-extension: tls
  endian: be
seq:
  - id: record_type
    type: u1
  - id: legacy_record_version
    type: u2
  - id: record_length
    type: u2
  - id: handshake_type
    type: u1
  - id: handshake_length
    type: u3
  - id: legacy_version
    type: u2
  - id: random
    type: random_struct
  - id: legacy_session_id
    type: session_id_struct
  - id: cipher_suites
    type: cipher_suites_struct
  - id: legacy_compression_methods
    type: compression_methods_struct
  - id: extensions_length
    type: u2
  - id: extensions
    type: extensions_struct
    size: extensions_length
types:
  random_struct:
    seq:
      - id: gmt_unix_time
        type: u4
      - id: random_bytes
        type: u1
        repeat: expr
        repeat-expr: 28
  session_id_struct:
    seq:
      - id: session_id_length
        type: u1
      - id: session_id
        type: u1
        repeat: expr
        repeat-expr: session_id_length
  cipher_suites_struct:
    seq:
      - id: cipher_suites_length
        type: u2
      - id: cipher_suites_list
        type: u2
        repeat: expr
        repeat-expr: cipher_suites_length / 2
  compression_methods_struct:
    seq:
      - id: compression_methods_length
        type: u1
      - id: compression_methods
        type: u1
        repeat: expr
        repeat-expr: compression_methods_length
  extensions_struct:
    seq:
      - id: extension_type
        type: u2
      - id: extension_length
        type: u2
      - id: extension_data
        type: u1
        repeat: expr
        repeat-expr: extension_length
    instances:
      root:
        pos: 0
        io: _root._io
        value: _parent