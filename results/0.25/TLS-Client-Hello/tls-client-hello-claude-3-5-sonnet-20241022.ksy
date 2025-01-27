meta:
  id: tls_client_hello
  file-extension: bin
  endian: be

seq:
  - id: record_header
    type: record_header
  - id: handshake_header
    type: handshake_header
  - id: client_version
    type: version
  - id: random
    type: random
  - id: session_id
    type: session_id
  - id: cipher_suites
    type: cipher_suites
  - id: compression_methods
    type: compression_methods
  - id: extensions
    type: extensions
    if: handshake_header.length > (2 + 32 + 1 + session_id.length + 2 + cipher_suites.length + 2 + compression_methods.length)

types:
  record_header:
    seq:
      - id: content_type
        type: u1
      - id: version
        type: version
      - id: length
        type: u2

  handshake_header:
    seq:
      - id: msg_type
        type: u1
      - id: length
        type: u3

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
        if: length > 0

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
      - id: methods
        size: length

  extensions:
    seq:
      - id: length
        type: u2
      - id: extensions_data
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