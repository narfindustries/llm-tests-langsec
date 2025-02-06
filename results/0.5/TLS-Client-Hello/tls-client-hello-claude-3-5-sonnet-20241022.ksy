meta:
  id: tls_client_hello
  title: TLS 1.3 Client Hello Message
  file-extension: bin
  endian: be

seq:
  - id: protocol_version
    type: u2
    valid: 0x0303
  - id: random
    size: 32
  - id: legacy_session_id
    type: legacy_session_id_t
  - id: cipher_suites
    type: cipher_suites_t
  - id: legacy_compression_methods
    type: legacy_compression_methods_t
  - id: extensions
    type: extensions_t

types:
  legacy_session_id_t:
    seq:
      - id: len
        type: u1
      - id: session_id
        size: len

  cipher_suites_t:
    seq:
      - id: len
        type: u2
      - id: cipher_suites
        type: u2
        repeat: expr
        repeat-expr: len / 2

  legacy_compression_methods_t:
    seq:
      - id: len
        type: u1
      - id: compression_methods
        size: len

  extensions_t:
    seq:
      - id: len
        type: u2
      - id: extensions
        type: extension
        repeat: eos

  extension:
    seq:
      - id: type
        type: u2
        enum: extension_type
      - id: len
        type: u2
      - id: body
        size: len
        type:
          switch-on: type
          cases:
            'extension_type::server_name': server_name_extension
            'extension_type::supported_groups': supported_groups_extension
            'extension_type::signature_algorithms': signature_algorithms_extension
            'extension_type::key_share': key_share_extension
            'extension_type::supported_versions': supported_versions_extension
            'extension_type::psk_key_exchange_modes': psk_key_exchange_modes_extension
            'extension_type::pre_shared_key': pre_shared_key_extension
            'extension_type::early_data': early_data_extension
            'extension_type::cookie': cookie_extension
            'extension_type::alpn': alpn_extension

  server_name_extension:
    seq:
      - id: len
        type: u2
      - id: server_name_list
        size: len
        type: server_name_list_t

  server_name_list_t:
    seq:
      - id: name_type
        type: u1
      - id: name_len
        type: u2
      - id: name
        size: name_len

  supported_groups_extension:
    seq:
      - id: len
        type: u2
      - id: groups
        type: u2
        repeat: expr
        repeat-expr: len / 2

  signature_algorithms_extension:
    seq:
      - id: len
        type: u2
      - id: algorithms
        type: u2
        repeat: expr
        repeat-expr: len / 2

  key_share_extension:
    seq:
      - id: len
        type: u2
      - id: client_shares
        type: key_share_entry
        repeat: eos

  key_share_entry:
    seq:
      - id: group
        type: u2
      - id: key_exchange_len
        type: u2
      - id: key_exchange
        size: key_exchange_len

  supported_versions_extension:
    seq:
      - id: len
        type: u1
      - id: versions
        type: u2
        repeat: expr
        repeat-expr: len / 2

  psk_key_exchange_modes_extension:
    seq:
      - id: len
        type: u1
      - id: modes
        size: len

  pre_shared_key_extension:
    seq:
      - id: identities_len
        type: u2
      - id: identities
        type: psk_identity
        repeat: eos
      - id: binders_len
        type: u2
      - id: binders
        type: psk_binder
        repeat: eos

  psk_identity:
    seq:
      - id: identity_len
        type: u2
      - id: identity
        size: identity_len
      - id: obfuscated_ticket_age
        type: u4

  psk_binder:
    seq:
      - id: len
        type: u1
      - id: binder
        size: len

  early_data_extension:
    seq: []

  cookie_extension:
    seq:
      - id: cookie_len
        type: u2
      - id: cookie
        size: cookie_len

  alpn_extension:
    seq:
      - id: len
        type: u2
      - id: protocol_name_list
        size: len
        type: protocol_name_list_t

  protocol_name_list_t:
    seq:
      - id: protocol_name_len
        type: u1
      - id: protocol_name
        size: protocol_name_len

enums:
  extension_type:
    0x0000: server_name
    0x000a: supported_groups
    0x000d: signature_algorithms
    0x0010: alpn
    0x0029: pre_shared_key
    0x002a: early_data
    0x002b: supported_versions
    0x002c: cookie
    0x002d: psk_key_exchange_modes
    0x0033: key_share