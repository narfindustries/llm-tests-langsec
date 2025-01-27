# This is a sample Kaitai Struct definition.  You'll need to replace this with your actual definition.
# The error message suggests a problem with the path or the ksy file itself.  This example is for illustration.

$schema: https://kaitai.io/schema/draft-04
id: tls_client_hello
endian: be

seq:
  - id: record_layer
    type: seq
    contents:
      - id: content_type
        type: u2
      - id: version
        type: u2
      - id: length
        type: u2be
      - id: fragment
        type: bytes
        size: length

  - id: client_hello
    type: seq
    contents:
      - id: client_version
        type: u2
      - id: random
        type: u8
        size: 32
      - id: session_id_length
        type: u8
      - id: session_id
        type: bytes
        size: session_id_length
      - id: cipher_suites_length
        type: u2
      - id: cipher_suites
        type: u2
        size: cipher_suites_length / 2
      - id: compression_methods_length
        type: u8
      - id: compression_methods
        type: u8
        size: compression_methods_length
      - id: extensions_length
        type: u2
      - id: extensions
        type: bytes
        size: extensions_length

