meta:
  id: tls_client_hello
  endian: be
seq:
  - id: handshake_type
    type: u1
    valid: 0x01
  - id: length
    type: length_3_bytes
  - id: version
    type: u2
    valid: 0x0303
  - id: random
    size: 32
  - id: session_id_length
    type: u1
  - id: session_id
    size: session_id_length
  - id: cipher_suites_length
    type: u2
  - id: cipher_suites
    type: cipher_suite
    repeat: expr
    repeat-expr: cipher_suites_length / 2
  - id: compression_methods_length
    type: u1
  - id: compression_methods
    type: u1
    repeat: expr
    repeat-expr: compression_methods_length
  - id: extensions_length
    type: u2
  - id: extensions
    type: extension
    repeat: expr
    repeat-expr: extensions_length
types:
  length_3_bytes:
    seq:
      - id: b1
        type: u1
      - id: b2
        type: u1
      - id: b3
        type: u1
    instances:
      value:
        value: (b1 << 16) | (b2 << 8) | b3
  cipher_suite:
    seq:
      - id: value
        type: u2
  extension:
    seq:
      - id: type
        type: u2
      - id: length
        type: u2
      - id: body
        type:
          switch-on: type
          cases:
            0x0000: server_name_ext
            0x0010: supported_groups_ext
            0x0011: ec_point_formats_ext
            0x002b: supported_versions_ext
            0x002d: psk_key_exchange_modes_ext
            0x0033: key_share_ext
  server_name_ext:
    seq:
      - id: list_length
        type: u2
      - id: server_names
        type: server_name
        repeat: expr
        repeat-expr: '_parent.length / 3'
  server_name:
    seq:
      - id: name_type
        type: u1
      - id: length
        type: u2
      - id: hostname
        type: str
        size: length
        encoding: ascii
  supported_groups_ext:
    seq:
      - id: groups_length
        type: u2
      - id: groups
        type: u2
        repeat: expr
        repeat-expr: groups_length / 2
  supported_versions_ext:
    seq:
      - id: versions_length
        type: u1
      - id: versions
        type: u2
        repeat: expr
        repeat-expr: versions_length / 2
  psk_key_exchange_modes_ext:
    seq:
      - id: modes_length
        type: u1
      - id: modes
        type: u1
        repeat: expr
        repeat-expr: modes_length
  key_share_ext:
    seq:
      - id: client_shares_length
        type: u2
      - id: client_shares
        type: key_share_entry
        repeat: expr
        repeat-expr: '_parent.length / 4'
  key_share_entry:
    seq:
      - id: group
        type: u2
      - id: key_exchange_length
        type: u2
      - id: key_exchange
        size: key_exchange_length