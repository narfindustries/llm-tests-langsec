meta:
  id: tls_client_hello
  file-extension: tls
  endian: be
seq:
  - id: legacy_version
    type: u2
    doc: "Fixed to 0x0303 (TLS 1.2) for backward compatibility."
  - id: random
    type: random
  - id: legacy_session_id
    type: session_id
  - id: cipher_suites
    type: cipher_suites
  - id: legacy_compression_methods
    type: compression_methods
  - id: extensions
    type: extensions
types:
  random:
    seq:
      - id: gmt_unix_time
        type: u4
        doc: "Current time in seconds since the Unix epoch."
      - id: random_bytes
        size: 28
        doc: "28 random bytes."
  session_id:
    seq:
      - id: length
        type: u1
        doc: "Length of the session ID (0 to 32 bytes)."
      - id: session_id
        size: length
        doc: "Session ID data."
  cipher_suites:
    seq:
      - id: length
        type: u2
        doc: "Length of the cipher suites list."
      - id: cipher_suites
        type: u2
        repeat: expr
        repeat-expr: length / 2
        doc: "List of cipher suites."
  compression_methods:
    seq:
      - id: length
        type: u1
        doc: "Length of the compression methods list."
      - id: compression_methods
        size: length
        doc: "List of compression methods (fixed to 0x00 in TLS 1.3)."
  extensions:
    seq:
      - id: length
        type: u2
        doc: "Length of the extensions list."
      - id: extensions
        type: extension
        repeat: eos
        doc: "List of extensions."
  extension:
    seq:
      - id: type
        type: u2
        doc: "Type of the extension."
      - id: length
        type: u2
        doc: "Length of the extension data."
      - id: data
        size: length
        doc: "Extension data."