meta:
  id: tls_client_hello
  title: TLS Client Hello
  license: MIT
  ks-version: 0.9
  endian: be
seq:
  - id: record_type
    type: u1
  - id: version
    type: u2
  - id: length
    type: u2
  - id: handshake_type
    type: u1
  - id: handshake_length
    type: u3
  - id: client_version
    type: u2
  - id: random
    type: random
    size: 32
  - id: session_id_length
    type: u1
  - id: session_id
    type: str
    size: session_id_length
    encoding: UTF-8
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
    repeat-expr: extensions_length
types:
  random:
    seq:
      - id: gmt_unix_time
        type: u4
      - id: random_bytes
        type: u1
        size: 28
  extension:
    seq:
      - id: type
        type: u2
      - id: length
        type: u2
      - id: data
        type: u1
        size: length