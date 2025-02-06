meta:
  id: tls_client_hello
  title: TLS ClientHello
  application: TLS
  file-extension: tls
  endian: be

seq:
  - id: client_version
    type: u2
  - id: random
    size: 32
  - id: legacy_session_id
    type: session_id

  - id: cipher_suites_length
    type: u2
  - id: cipher_suites
    type: u2
    repeat: expr
    repeat-expr: cipher_suites_length / 2

  - id: legacy_compression_methods_length
    type: u1
  - id: legacy_compression_methods
    repeat: expr
    repeat-expr: legacy_compression_methods_length
    type: u1

  - id: extensions_length
    type: u2
  - id: extensions
    type: extension
    repeat: expr
    repeat-expr: extensions_length

types:
  session_id:
    seq:
      - id: length
        type: u1
      - id: value
        size: length

  extension:
    seq:
      - id: type
        type: u2
      - id: length
        type: u2
      - id: data
        size: length