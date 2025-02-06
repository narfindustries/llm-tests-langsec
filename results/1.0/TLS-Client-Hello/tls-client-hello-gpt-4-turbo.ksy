meta:
  id: tls_client_hello
  title: TLS Client Hello Message
  file-extension: tls
  endian: be
doc: |
  The structure of a TLS ClientHello message based on RFC 8446.
seq:
  - id: legacy_record_version
    type: u2
  - id: random
    type: random
  - id: legacy_session_id_length
    type: u1
  - id: legacy_session_id
    size: legacy_session_id_length
    type: u1
    repeat: expr
    repeat-expr: legacy_session_id_length
  - id: cipher_suites_length
    type: u2
  - id: cipher_suites
    type: cipher_suites
    size: cipher_suites_length
  - id: legacy_compression_methods_length
    type: u1
  - id: legacy_compression_methods
    type: u1
    repeat: expr
    repeat-expr: legacy_compression_methods_length
  - id: extensions_length
    type: u2
  - id: extensions
    type: extension
    repeat: expr
    repeat-expr: (extensions_length - current_size) / (typical_extension_size)
types:
  random:
    seq:
      - id: gmt_unix_time
        type: u4
      - id: random_bytes
        size: 28
  cipher_suites:
    seq:
      - id: suites
        type: u2
        repeat: expr
        repeat-expr: (cipher_suites_length - 2) / 2
  extension:
    seq:
      - id: extension_type
        type: u2
      - id: extension_length
        type: u2
      - id: extension_data
        size: extension_length