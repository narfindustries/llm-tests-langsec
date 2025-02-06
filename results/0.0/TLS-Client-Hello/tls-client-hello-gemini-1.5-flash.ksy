types:
  tls_client_hello:
    seq:
      - id: protocol_version
        type: u2
      - id: random
        type: bytes
        size: 32
      - id: session_id
        type: bytes
      - id: cipher_suites
        type: array
        type: u2
        size: lambda: self.cipher_suites_len
      - id: compression_methods
        type: array
        type: u1
        size: lambda: self.compression_methods_len
      - id: extensions
        type: array
        type: tls_extension
        size: lambda: self.extensions_len
    instances:
      - cipher_suites_len: lambda: len(self.cipher_suites)
      - compression_methods_len: lambda: len(self.compression_methods)
      - extensions_len: lambda: len(self.extensions)

  tls_extension:
    seq:
      - id: extension_type
        type: u2
      - id: extension_data
        type: bytes
        size: lambda: self.extension_data_len
    instances:
      - extension_data_len: lambda: self.extension_data_len_calc()

    calc:
      extension_data_len_calc:
        code: |
          # Placeholder - Replace with actual length calculation based on extension_type
          return 0

