meta:
  id: tls_client_hello
  title: TLS Client Hello Message
  endian: be
  xref:
    rfc: 8446

seq:
  - id: legacy_version
    type: u2
  - id: random
    size: 32
  - id: len_legacy_session_id
    type: u1
  - id: legacy_session_id
    size: len_legacy_session_id
  - id: cipher_suites_length
    type: u2
  - id: cipher_suites
    type: cipher_suite
    repeat: expr
    repeat-expr: cipher_suites_length / 2
  - id: len_legacy_compression_methods
    type: u1
  - id: legacy_compression_methods
    size: len_legacy_compression_methods
  - id: num_extensions
    type: u2
  - id: extensions
    type: tls_extension
    repeat: expr
    repeat-expr: num_extensions

types:
  cipher_suite:
    seq:
      - id: suite
        type: u2

  tls_extension:
    seq:
      - id: type
        type: u2
      - id: length
        type: u2
      - id: data
        size: length
        type:
          switch-on: type
          cases:
            0: server_name_list
            10: supported_groups_list
            13: signature_algorithms_list
            43: supported_versions_list
            45: psk_key_exchange_modes
            51: key_share_list
            42: early_data

  server_name_list:
    seq:
      - id: list_length
        type: u2
      - id: server_names
        type: server_name
        repeat: expr
        repeat-expr: list_length

  server_name:
    seq:
      - id: name_type
        type: u1
      - id: name_length
        type: u2
      - id: name
        type: str
        encoding: UTF-8
        size: name_length

  supported_groups_list:
    seq:
      - id: list_length
        type: u2
      - id: groups
        type: u2
        repeat: expr
        repeat-expr: list_length / 2

  signature_algorithms_list:
    seq:
      - id: list_length
        type: u2
      - id: signatures
        type: u2
        repeat: expr
        repeat-expr: list_length / 2

  supported_versions_list:
    seq:
      - id: list_length
        type: u1
      - id: versions
        type: u2
        repeat: expr
        repeat-expr: list_length / 2

  psk_key_exchange_modes:
    seq:
      - id: num_modes
        type: u1
      - id: modes
        type: u1
        repeat: expr
        repeat-expr: num_modes

  key_share_list:
    seq:
      - id: list_length
        type: u2
      - id: key_shares
        type: key_share
        repeat: expr
        repeat-expr: list_length

  key_share:
    seq:
      - id: group
        type: u2
      - id: len_key_exchange
        type: u2
      - id: key_exchange
        size: len_key_exchange

  early_data:
    seq:
      - id: max_early_data_size
        type: u4
        if: _parent.length == 4