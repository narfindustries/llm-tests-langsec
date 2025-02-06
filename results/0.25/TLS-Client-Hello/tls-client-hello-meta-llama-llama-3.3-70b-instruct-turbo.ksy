meta:
  id: tls_client_hello
  endian: be

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
    repeat: repeat-expr
    repeat-expr: cipher_suites_length / 2
  - id: legacy_compression_methods_length
    type: uint8
  - id: legacy_compression_methods
    type: uint8
    repeat: repeat-expr
    repeat-expr: legacy_compression_methods_length
  - id: extensions_length
    type: uint16
  - id: extensions
    type: extension
    repeat: until
    until: _io.pos >= _io.size

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
      - id: extension_data
        type:
          switch-on: type
          cases:
            0: server_name
            43: supported_versions
            41: supported_groups
            40: key_share
            45: pre_shared_key
            42: early_data
            else: bytes

  server_name:
    seq:
      - id: name_type
        type: uint8
      - id: length
        type: uint16
      - id: host_name
        type: string
        encoding: utf-8

  supported_versions:
    seq:
      - id: versions
        type: uint16
        repeat: until
        until: _io.pos >= _io.size - length

  supported_groups:
    seq:
      - id: groups
        type: uint16
        repeat: until
        until: _io.pos >= _io.size - length

  key_share:
    seq:
      - id: client_shares
        type: key_share_entry
        repeat: until
        until: _io.pos >= _io.size - length

  key_share_entry:
    seq:
      - id: group
        type: uint16
      - id: key_exchange_length
        type: uint16
      - id: key_exchange
        type: bytes
        size: key_exchange_length

  pre_shared_key:
    seq:
      - id: identities
        type: pre_shared_key_identity
        repeat: until
        until: _io.pos >= _io.size - length
      - id: binders
        type: bytes
        repeat: until
        until: _io.pos >= _io.size - length

  pre_shared_key_identity:
    seq:
      - id: identity_length
        type: uint16
      - id: identity
        type: bytes
        size: identity_length
      - id: obfuscated_ticket_age
        type: uint32

  early_data:
    seq:
      - id: max_early_data_size
        type: uint32