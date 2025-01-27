meta:
  id: tls-client-hello
  title: TLS Client Hello
  endian: le

types:
  tls_version:
    seq:
      - id: major
        type: u1
      - id: minor
        type: u1

  random:
    seq:
      - id: gmt_unix_time
        type: u4
      - id: random_bytes
        type: u3

  session_id:
    seq:
      - id: len
        type: u1
      - id: session_id
        type: bytes
        size: len

  cipher_suite:
    seq:
      - id: value
        type: u2

  compression_method:
    seq:
      - id: value
        type: u1

  extension:
    seq:
      - id: type
        type: u2
      - id: len
        type: u2
      - id: data
        type: bytes
        size: len

seq:
  - id: record_layer_version
    type: tls_version
  - id: record_layer_length
    type: u2
  - id: handshake_type
    type: u1
  - id: handshake_length
    type: u3
  - id: handshake_version
    type: tls_version
  - id: random
    type: random
  - id: session_id
    type: session_id
  - id: cipher_suites
    type: cipher_suite
    repeat: expr
    repeat-expr: (handshake_length - 43) / 2
  - id: compression_methods
    type: compression_method
    repeat: expr
    repeat-expr: handshake_length - 43 - (cipher_suites.length * 2)
  - id: extensions
    type: extension
    repeat: until
    repeat-until: _io.pos >= (_io.size - 4)