meta:
  id: tls_client_hello
  title: TLS Client Hello
  file-extension: tls
  endian: be

seq:
  - id: record_length
    type: u2
  - id: message_type
    type: u1
  - id: version
    type: version
  - id: random
    type: random
  - id: session_id_length
    type: u1
  - id: session_id
    size: session_id_length
  - id: cipher_suites_length
    type: u2
  - id: cipher_suites
    type: cipher_suites
  - id: compression_methods_length
    type: u1
  - id: compression_methods
    size: compression_methods_length
  - id: extensions_length
    type: u2
  - id: extensions
    size: extensions_length

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

  cipher_suites:
    seq:
      - id: cipher_suites
        type: u2
        repeat: eos