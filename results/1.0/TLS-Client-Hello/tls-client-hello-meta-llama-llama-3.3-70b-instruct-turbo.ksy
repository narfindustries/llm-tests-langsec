meta:
  id: tls_client_hello
  title: TLS Client Hello
seq:
  - id: legacy_version
    type: uint16
  - id: random
    type: bytes
    size: 32
  - id: legacy_session_id_length
    type: uint8
  - id: legacy_session_id
    type: bytes
    size: legacy_session_id_length
  - id: cipher_suites_length
    type: uint16
  - id: cipher_suites
    type: seq
    repeat: expr
    repeat-expr: id: cipher_suites_count, value: cipher_suites_length / 2
    seq:
      - id: cipher_suite
        type: uint16
  - id: legacy_compression_methods_length
    type: uint8
  - id: legacy_compression_methods
    type: bytes
    size: legacy_compression_methods_length
  - id: extensions_length
    type: uint16
  - id: extensions
    type: seq
    repeat: until
    until: _io.pos + extensions_length == _io.size
    seq:
      - id: extension_type
        type: uint16
      - id: extension_length
        type: uint16
      - id: extension_data
        type:
          switch-on: extension_type
          cases:
            43:
              type: seq
              repeat: expr
              repeat-expr: id: supported_versions_count, value: extension_length / 2
              seq:
                - id: supported_version
                  type: uint16
            10:
              type: seq
              repeat: expr
              repeat-expr: id: supported_groups_count, value: (extension_length - 2) / 2
              seq:
                - id: supported_group
                  type: uint16
            51:
              type: seq
              repeat: expr
              repeat-expr: id: key_share_count, value: extension_length / (2 + 32 + 2)
              seq:
                - id: key_share_entry
                  type:
                    seq:
                      - id: group
                        type: uint16
                      - id: key_exchange_length
                        type: uint16
                      - id: key_exchange
                        type: bytes
                        size: key_exchange_length
            13:
              type: seq
              repeat: expr
              repeat-expr: id: signature_algorithms_count, value: (extension_length - 2) / 2
              seq:
                - id: signature_algorithm
                  type: uint16
            41:
              type: seq
              repeat: expr
              repeat-expr: id: pre_shared_key_count, value: extension_length / (2 + 32 + 1)
              seq:
                - id: pre_shared_key_id
                  type: uint16
                - id: obfuscated_ticket_age
                  type: bytes
                  size: 32
            else:
              type: bytes
              size: extension_length - 2