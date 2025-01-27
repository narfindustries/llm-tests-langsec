meta:
  id: tls_client_hello
  title: TLS Client Hello
  file-extension: dat
  endian: be
  license: CC0-1.0
  ks-version: 0.9

doc: |
  The TLS Client Hello message, sent by a client to initiate the TLS handshake process.

types:
  record:
    seq:
      - id: type
        type: u1
        enum: handshake_type
      - id: length
        type: u3
      - id: handshake_version
        type: version
      - id: random
        size: 32
      - id: session_id
        type: prefixed_array

      - id: cipher_suites
        type: cipher_suites

      - id: compression_methods
        type: compression_methods

  prefixed_array:
    seq:
      - id: length
        type: u1
      - id: data
        size: length
  cipher_suites:
    seq:
      - id: length
        type: u2
      - id: suites
        type: u2
        repeat: expr
        repeat-expr: length // 2

  compression_methods:
    seq:
      - id: length
        type: u1
      - id: methods
        type: u1
        repeat: expr
        repeat-expr: length

  version:
    seq:
      - id: major
        type: u1
      - id: minor
        type: u1

enums:
  handshake_type:
    1: client_hello

