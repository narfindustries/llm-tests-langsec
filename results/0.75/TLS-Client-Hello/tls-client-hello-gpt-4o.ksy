meta:
  id: tls_client_hello
  title: TLS Client Hello
  file-extension: bin
  endian: be

seq:
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
    type: cipher_suite
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
  cipher_suite:
    doc: Supported cipher suites
    seq:
      - id: cipher
        type: u2

  extension:
    doc: Supported extensions
    seq:
      - id: extension_type
        type: u2

      - id: extension_length
        type: u2

      - id: extension_data
        size: extension_length