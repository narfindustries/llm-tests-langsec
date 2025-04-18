meta:
  id: tls_client_hello
  title: TLS Client Hello
  license: CC0-1.0
  endian: be
seq:
  - id: legacy_version
    type: u2
    doc: The highest version of TLS supported by the client.
  - id: random
    size: 32
    doc: 32-byte random value generated by the client.
  - id: legacy_session_id
    type: session_id
    doc: Session ID from a previous session.
  - id: cipher_suites
    type: cipher_suites
    doc: List of cryptographic cipher suites supported by the client.
  - id: legacy_compression_methods
    type: compression_methods
    doc: Legacy field for compression methods.
  - id: extensions
    type: extensions
    doc: List of extensions to provide additional functionality.
types:
  session_id:
    seq:
      - id: length
        type: u1
      - id: session_id
        size: length
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
      - id: compression_methods
        type: u1
        repeat: expr
        repeat-expr: length
  extensions:
    seq:
      - id: length
        type: u2
      - id: extensions
        type: extension
        repeat: until
        repeat-until: _.is_last
  extension:
    seq:
      - id: extension_type
        type: u2
      - id: length
        type: u2
      - id: data
        size: length
    instances:
      is_last:
        value: _io.pos >= _parent._io.size