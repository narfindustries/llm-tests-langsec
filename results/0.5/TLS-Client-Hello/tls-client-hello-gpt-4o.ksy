meta:
  id: tls_client_hello
  title: TLS ClientHello
  application: tls
  file-extension: tls
  endian: be

seq:
  - id: client_version
    type: version

  - id: random
    type: random

  - id: legacy_session_id
    type: session_id

  - id: cipher_suites
    type: cipher_suites

  - id: legacy_compression_methods
    type: compression_methods

  - id: extensions
    type: extensions

types:
  version:
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
        size: 28

  session_id:
    seq:
      - id: length
        type: u1
      - id: session_id
        size: length

  cipher_suites:
    seq:
      - id: length
        type: u2
      - id: cipher_suites
        type: u2
        repeat: expr
        repeat-expr: length / 2

  compression_methods:
    seq:
      - id: length
        type: u1
      - id: compression_methods
        type: u1
        repeat: expr
        repeat-expr: length

  extensions:
    seq:
      - id: length
        type: u2
      - id: extensions
        size: length
        type: extension
        repeat: eos

  extension:
    seq:
      - id: type
        type: u2
      - id: length
        type: u2
      - id: data
        size: length