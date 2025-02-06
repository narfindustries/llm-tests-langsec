types:
  tls_client_hello:
    seq:
      - id: client_version
        type: u2be
      - id: random
        type: bytes
        size: 32
      - id: session_id_len
        type: u1
      - id: session_id
        type: bytes
        size: (session_id_len)
      - id: cipher_suites_len
        type: u2be
      - id: cipher_suites
        type: bytes
        size: (cipher_suites_len)
      - id: compression_methods_len
        type: u1
      - id: compression_methods
        type: bytes
        size: (compression_methods_len)
      - id: extensions_len
        type: u2be
      - id: extensions
        type: array
        size: (extensions_len)
        elements: tls_extension

  tls_extension:
    seq:
      - id: extension_type
        type: u2be
      - id: extension_data_len
        type: u2be
      - id: extension_data
        type: bytes
        size: (extension_data_len)

