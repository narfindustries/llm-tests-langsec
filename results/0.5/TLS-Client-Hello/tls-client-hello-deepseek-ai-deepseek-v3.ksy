meta:
  id: tls_client_hello
  title: TLS Client Hello
  file-extension: tls
  license: MIT
  endian: be
seq:
  - id: legacy_version
    type: u2
  - id: random
    size: 32
  - id: legacy_session_id
    type: session_id
  - id: cipher_suites
    type: cipher_suites
  - id: legacy_compression_methods
    type: compression_methods
  - id: extensions
    type: extensions
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
        repeat: eos
  extension:
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
            0x0000: server_name
            0x000a: supported_groups
            0x000d: signature_algorithms
            0x0010: alpn
            0x0029: pre_shared_key
            0x002b: supported_versions
            0x002d: psk_key_exchange_modes
            0x0033: key_share
  server_name:
    seq:
      - id: list_length
        type: u2
      - id: server_name_list
        type: server_name_entry
        repeat: eos
  server_name_entry:
    seq:
      - id: name_type
        type: u1
      - id: length
        type: u2
      - id: host_name
        size: length
  supported_groups:
    seq:
      - id: list_length
        type: u2
      - id: supported_groups
        type: u2
        repeat: expr
        repeat-expr: list_length / 2
  signature_algorithms:
    seq:
      - id: list_length
        type: u2
      - id: signature_algorithms
        type: u2
        repeat: expr
        repeat-expr: list_length / 2
  alpn:
    seq:
      - id: list_length
        type: u2
      - id: protocol_name_list
        type: protocol_name
        repeat: eos
  protocol_name:
    seq:
      - id: length
        type: u1
      - id: name
        size: length
  pre_shared_key:
    seq:
      - id: identities_length
        type: u2
      - id: identities
        type: psk_identity
        repeat: expr
        repeat-expr: identities_length
      - id: binders_length
        type: u2
      - id: binders
        type: psk_binder
        repeat: expr
        repeat-expr: binders_length
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
  supported_versions:
    seq:
      - id: list_length
        type: u1
      - id: supported_versions
        type: u2
        repeat: expr
        repeat-expr: list_length / 2
  psk_key_exchange_modes:
    seq:
      - id: list_length
        type: u1
      - id: modes
        type: u1
        repeat: expr
        repeat-expr: list_length
  key_share:
    seq:
      - id: client_shares_length
        type: u2
      - id: client_shares
        type: key_share_entry
        repeat: expr
        repeat-expr: client_shares_length
  key_share_entry:
    seq:
      - id: group
        type: u2
      - id: key_exchange_length
        type: u2
      - id: key_exchange
        size: key_exchange_length