seq:
  - id: legacy_version
    type: uint16
  - id: random
    type: bytes
    size: 32
  - id: legacy_session_id_len
    type: uint8
  - id: legacy_session_id
    type: bytes
    size: legacy_session_id_len
  - id: cipher_suites_len
    type: uint16
  - id: cipher_suites
    type: cipher_suite
    repeat: expr
    repeat-expr: cipher_suites_len / 2
  - id: legacy_compression_methods_len
    type: uint8
  - id: legacy_compression_methods
    type: uint8
    repeat: expr
    repeat-expr: legacy_compression_methods_len
  - id: extensions_len
    type: uint16
  - id: extensions
    type: extension
    repeat: expr
    repeat-expr: extensions_len

types:
  cipher_suite:
    seq:
      - id: value
        type: uint16

  extension:
    seq:
      - id: type
        type: uint16
      - id: len
        type: uint16
      - id: data
        type: bytes
        size: len

    instances:
      supported_versions:
        seq:
          - id: type
            type: uint16
            value: 43
          - id: len
            type: uint16
          - id: versions
            type: uint16
            repeat: expr
            repeat-expr: len / 2
        size: expr
        size-expr: len + 4

      supported_groups:
        seq:
          - id: type
            type: uint16
            value: 10
          - id: len
            type: uint16
          - id: groups
            type: uint16
            repeat: expr
            repeat-expr: len / 2
        size: expr
        size-expr: len + 4

      signature_algorithms:
        seq:
          - id: type
            type: uint16
            value: 13
          - id: len
            type: uint16
          - id: algorithms
            type: uint16
            repeat: expr
            repeat-expr: len / 2
        size: expr
        size-expr: len + 4

      key_share:
        seq:
          - id: type
            type: uint16
            value: 51
          - id: len
            type: uint16
          - id: client_shares
            type: client_share
            repeat: expr
            repeat-expr: len

        size: expr
        size-expr: len + 4

      client_share:
        seq:
          - id: group
            type: uint16
          - id: key_exchange_len
            type: uint16
          - id: key_exchange
            type: bytes
            size: key_exchange_len
        size: expr
        size-expr: 2 + 2 + key_exchange_len

      psk_key_exchange_modes:
        seq:
          - id: type
            type: uint16
            value: 45
          - id: len
            type: uint16
          - id: modes
            type: uint8
            repeat: expr
            repeat-expr: len
        size: expr
        size-expr: len + 4

      early_data:
        seq:
          - id: type
            type: uint16
            value: 42
          - id: len
            type: uint16
          - id: max_early_data_size
            type: uint32
            if: len == 4
        size: expr
        size-expr: len + 4