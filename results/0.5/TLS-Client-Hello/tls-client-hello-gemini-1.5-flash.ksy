types:
  tls_client_hello:
    seq:
      - id: protocol_version
        type: u2be
      - id: random
        type: bytes
        size: 32
      - id: session_id
        type: bytes
      - id: cipher_suites
        type: array
        size: 0
        type: u2be
      - id: compression_methods
        type: array
        size: 0
        type: u1
      - id: extensions
        type: array
        size: 0
        type: tls_extension

  tls_extension:
    seq:
      - id: extension_type
        type: u2be
      - id: extension_data
        type: bytes
