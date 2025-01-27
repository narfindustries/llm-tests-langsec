meta:
  id: tls_client_hello
  title: TLS Client Hello
  endian: be

seq:
  - id: handshake
    type: handshake

types:
  handshake:
    seq:
      - id: handshake_type
        type: u1
      - id: length
        type: u3
      - id: body
        size: length
        type:
          switch-on: handshake_type
          cases:
            0x01: client_hello

  client_hello:
    seq:
      - id: version
        type: u2
      - id: random
        type: random
      - id: session_id_length
        type: u1
      - id: session_id
        size: session_id_length
      - id: cipher_suites_length
        type: u2
      - id: cipher_suites
        size: cipher_suites_length
        type: cipher_suite
        repeat: eos
      - id: compression_methods_length
        type: u1
      - id: compression_methods
        size: compression_methods_length
        type: compression_method
        repeat: eos
      - id: extensions_length
        type: u2
      - id: extensions
        size: extensions_length
        type: extension
        repeat: eos

  random:
    seq:
      - id: gmt_unix_time
        type: u4
      - id: random_bytes
        size: 28

  cipher_suite:
    seq:
      - id: cipher_suite
        type: u2

  compression_method:
    seq:
      - id: compression_method
        type: u1

  extension:
    seq:
      - id: extension_type
        type: u2
      - id: extension_length
        type: u2
      - id: extension_data
        size: extension_length