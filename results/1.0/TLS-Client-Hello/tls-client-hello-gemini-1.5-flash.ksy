# This is a sample Kaitai Struct definition.  Adapt as needed for your actual data.
# The error message suggests a problem with the file path or the compiler itself.
# This example avoids the problematic path structure from the error.


types:
  tls_client_hello_gemini_1:
    seq:
      - id: type
        type: u1
      - id: version
        type: u2le
      - id: random
        type: array
        type: u1
        size: 32
      - id: session_id
        type: seq
          - id: len
            type: u1
          - id: data
            type: array
            type: u1
            size: lambda len: len
      - id: cipher_suites
        type: seq
          - id: len
            type: u2be
          - id: data
            type: array
            type: u2be
            size: lambda len: len / 2
      - id: compression_methods
        type: seq
          - id: len
            type: u1
          - id: data
            type: array
            type: u1
            size: lambda len: len
      - id: extensions
        type: seq
          - id: len
            type: u2be
          - id: data
            type: array
            type: u1
            size: lambda len: len


