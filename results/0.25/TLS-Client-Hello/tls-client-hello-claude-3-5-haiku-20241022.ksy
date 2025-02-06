meta:
  id: tls_client_hello
  endian: be
seq:
  - id: handshake_type
    type: u1
    valid: 0x01
  - id: length
    type: u3
  - id: client_version
    type: u2
    valid: 0x0303
  - id: random
    size: 32
  - id: legacy_session_id_length
    type: u1
  - id: legacy_session_id
    size: legacy_session_id_length
  - id: cipher_suites_length
    type: u2
  - id: cipher_suites
    type: u2
    repeat: expr
    repeat-expr: cipher_suites_length / 2
  - id: legacy_compression_methods_length
    type: u1
  - id: legacy_compression_methods
    type: u1
    repeat: expr
    repeat-expr: legacy_compression_methods_length
  - id: extensions_length
    type: u2
  - id: extensions
    type: extension
    repeat: expr
    repeat-expr: extensions_length > 0 ? 1 : 0

types:
  extension:
    seq:
      - id: type
        type: u2
      - id: length
        type: u2
      - id: body
        size: length
        type: 
          switch-on: type
          cases:
            0x0000: server_name
            0x002B: supported_versions
            0x000D: signature_algorithms
            0x000A: supported_groups
            0x0033: key_share
            0x0029: pre_shared_key
            0x002D: psk_key_exchange_modes
        
  server_name:
    seq:
      - id: list_length
        type: u2
      - id: server_names
        type: server_name_entry
        repeat: expr
        repeat-expr: list_length > 0 ? 1 : 0

  server_name_entry:
    seq:
      - id: name_type
        type: u1
      - id: name_length
        type: u2
      - id: name
        size: name_length

  supported_versions:
    seq:
      - id: versions_length
        type: u1
      - id: versions
        type: u2
        repeat: expr
        repeat-expr: versions_length / 2

  signature_algorithms:
    seq:
      - id: algorithms_length
        type: u2
      - id: algorithms
        type: u2
        repeat: expr
        repeat-expr: algorithms_length / 2

  supported_groups:
    seq:
      - id: groups_length
        type: u2
      - id: groups
        type: u2
        repeat: expr
        repeat-expr: groups_length / 2

  key_share:
    seq:
      - id: client_shares_length
        type: u2
      - id: client_shares
        type: key_share_entry
        repeat: expr
        repeat-expr: client_shares_length > 0 ? 1 : 0

  key_share_entry:
    seq:
      - id: group
        type: u2
      - id: key_exchange_length
        type: u2
      - id: key_exchange
        size: key_exchange_length

  pre_shared_key:
    seq:
      - id: identities_length
        type: u2
      - id: identities
        type: psk_identity
        repeat: expr
        repeat-expr: identities_length > 0 ? 1 : 0
      - id: binders_length
        type: u2
      - id: binders
        type: psk_binder
        repeat: expr
        repeat-expr: binders_length > 0 ? 1 : 0

  psk_identity:
    seq:
      - id: identity_length
        type: u2
      - id: identity
        size: identity_length
      - id: obfuscated_ticket_age
        type: u4

  psk_binder:
    seq:
      - id: binder_length
        type: u1
      - id: binder
        size: binder_length

  psk_key_exchange_modes:
    seq:
      - id: modes_length
        type: u1
      - id: modes
        type: u1
        repeat: expr
        repeat-expr: modes_length