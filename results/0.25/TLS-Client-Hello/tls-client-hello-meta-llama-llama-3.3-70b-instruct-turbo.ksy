meta:
  id: tls_client_hello
  title: TLS Client Hello
  endian: be

seq:
  - id: record_content_type
    type: u1

  - id: record_version
    type: u2

  - id: record_length
    type: u2

  - id: handshake_type
    type: u1

  - id: handshake_length
    type: u3

  - id: handshake_version
    type: u2

  - id: random
    type: bytes
    size: 32

  - id: session_id_length
    type: u1

  - id: session_id
    type: bytes
    size: session_id_length

  - id: cipher_suites_length
    type: u2

  - id: cipher_suites
    type: cipher_suite
    repeat: expr
    repeat-expr: cipher_suites_length / 2

  - id: compression_methods_length
    type: u1

  - id: compression_methods
    type: u1
    repeat: expr
    repeat-expr: compression_methods_length

types:
  cipher_suite:
    seq:
      - id: suite
        type: u2