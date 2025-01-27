meta:
  id: tls_client_hello
  title: TLS Client Hello
  file-extension: tls
  endian: be
  license: CC0-1.0
  ks-version: 0.9

doc: |
  The TLS Client Hello message, sent by the client to initiate the TLS handshake process.

seq:
  - id: record_type
    type: u1
    enum: record_type
    doc: The type of the TLS record.

  - id: major_version
    type: u1
    doc: Major version of TLS.

  - id: minor_version
    type: u1
    doc: Minor version of TLS.

  - id: length
    type: u2
    doc: Length of the following TLS payload.

  - id: handshake_type
    type: u1
    enum: handshake_type
    doc: Type of handshake message.

  - id: handshake_length
    type: u3
    doc: Length of the handshake message.

  - id: client_version
    type: version
    doc: Version of the TLS protocol by the client.

  - id: random
    size: 32
    doc: Random structure.

  - id: session_id
    type: prefixed_array(u1, u1)
    doc: Session identifier.

  - id: cipher_suites
    type: prefixed_array(u2, u2)
    doc: List of cipher suites supported by the client.

  - id: compression_methods
    type: prefixed_array(u1, u1)
    doc: List of compression methods supported by the client.

  - id: extensions
    type: extensions
    doc: Optional extensions.

types:
  version:
    seq:
      - id: major
        type: u1
      - id: minor
        type: u1

  prefixed_array:
    params:
      - id: element_type
        type: kaitai::kstype
      - id: length_type
        type: kaitai::kstype
    seq:
      - id: length
        type: length_type
      - id: data
        type: element_type
        repeat: expr
        repeat-expr: length

  extensions:
    seq:
      - id: total_length
        type: u2
      - id: entries
        type: extension
        repeat: until
        repeat-until: _.total_length == 0

  extension:
    seq:
      - id: type
        type: u2
        enum: extension_type
      - id: length
        type: u2
      - id: data
        size: length

enums:
  record_type:
    0x16: handshake

  handshake_type:
    0x01: client_hello

  extension_type:
    0x0000: server_name
    0x000d: signature_algorithms
    0x0023: extended_master_secret
    0x0010: supported_groups
    0x0005: status_request
    0x000b: ec_point_formats