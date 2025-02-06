types:
  tls_client_hello:
    seq:
      - id: client_version
        type: u2
      - id: random
        type: bytes
        size: 32
      - id: session_id_len
        type: u1
      - id: session_id
        type: bytes
        size: session_id_len
      - id: cipher_suites_len
        type: u2
      - id: cipher_suites
        type: array
        type: u2
        size: cipher_suites_len
      - id: compression_methods_len
        type: u1
      - id: compression_methods
        type: array
        type: u1
        size: compression_methods_len
      - id: extensions_len
        type: u2
      - id: extensions
        type: array
        type: tls_extension
        size: extensions_len

  tls_extension:
    seq:
      - id: extension_type
        type: u2
      - id: extension_data_len
        type: u2
      - id: extension_data
        type: bytes
        size: extension_data_len


