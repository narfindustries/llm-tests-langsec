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
    type: cipher_suite
    repeat: expr
    repeat_expr: cipher_suites_length / 2
  - id: legacy_compression_methods_length
    type: uint8
  - id: legacy_compression_methods
    type: uint8
    repeat: expr
    repeat_expr: legacy_compression_methods_length
  - id: extensions_length
    type: uint16
  - id: extensions
    type: extension
    repeat: expr
    repeat_expr: extensions_length
types:
  cipher_suite:
    seq:
      - id: value
        type: uint16
  extension:
    seq:
      - id: type
        type: uint16
      - id: length
        type: uint16
      - id: data
        type: bytes
        size: length
        seq:
          - id: extension_data
            type:
              switch_on: type
              cases:
                0x0000: # server_name
                  type: server_name
                0x0001: # max_fragment_length
                  type: max_fragment_length
                0x0002: # client_certificate_url
                  type: client_certificate_url
                0x0003: # trusted_ca_keys
                  type: trusted_ca_keys
                0x0004: # truncated_hmac
                  type: truncated_hmac
                0x0005: # status_request
                  type: status_request
                0x0029: # pre_shared_key
                  type: pre_shared_key
                0x002b: # supported_versions
                  type: supported_versions
                0x0033: # key_share
                  type: key_share
                0x000a: # supported_groups
                  type: supported_groups
  server_name:
    seq:
      - id: name_type
        type: uint8
      - id: length
        type: uint16
      - id: host_name
        type: bytes
        size: length
  max_fragment_length:
    seq:
      - id: value
        type: uint8
  client_certificate_url:
    seq:
      - id: value
        type: bytes
        size: length
  trusted_ca_keys:
    seq:
      - id: value
        type: bytes
        size: length
  truncated_hmac:
    seq:
      - id: value
        type: uint8
  status_request:
    seq:
      - id: value
        type: bytes
        size: length
  pre_shared_key:
    seq:
      - id: identity_length
        type: uint16
      - id: identity
        type: bytes
        size: identity_length
      - id: obfuscated_ticket_age
        type: uint32
  supported_versions:
    seq:
      - id: versions
        type: uint16
        repeat: expr
        repeat_expr: length / 2
  key_share:
    seq:
      - id: group
        type: uint16
      - id: key_exchange_length
        type: uint16
      - id: key_exchange
        type: bytes
        size: key_exchange_length
  supported_groups:
    seq:
      - id: groups
        type: uint16
        repeat: expr
        repeat_expr: length / 2
  extension_list:
    seq:
      - id: extensions
        type: extension
        repeat: until
        until: _io.pos == _io.size