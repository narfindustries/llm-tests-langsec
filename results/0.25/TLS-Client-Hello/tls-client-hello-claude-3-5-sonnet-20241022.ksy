meta:
  id: tls_client_hello
  endian: be
enums:
  named_group:
    0x0017: secp256r1
    0x0018: secp384r1
    0x0019: secp521r1
    0x001d: x25519
    0x001e: x448
seq:
  - id: msg_type
    type: u1
    valid: 0x01
  - id: length
    type: b24
  - id: legacy_version
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
    type: cipher_suite
    repeat: expr
    repeat-expr: cipher_suites_length / 2
  - id: compression_methods_length
    type: u1
  - id: compression_methods
    size: compression_methods_length
  - id: extensions_length
    type: u2
  - id: extensions
    type: extension
    repeat: eos
types:
  cipher_suite:
    seq:
      - id: value
        type: u2
        enum: cipher_suite_enum
    enums:
      cipher_suite_enum:
        0x1301: tls_aes_128_gcm_sha256
        0x1302: tls_aes_256_gcm_sha384
        0x1303: tls_chacha20_poly1305_sha256
  extension:
    seq:
      - id: type
        type: u2
        enum: extension_type
      - id: length
        type: u2
      - id: body
        size: length
        type:
          switch-on: type
          cases:
            'extension_type::server_name': server_name_extension
            'extension_type::supported_groups': supported_groups_extension
            'extension_type::signature_algorithms': signature_algorithms_extension
            'extension_type::key_share': key_share_extension
            'extension_type::supported_versions': supported_versions_extension
            'extension_type::psk_key_exchange_modes': psk_modes_extension
            'extension_type::pre_shared_key': pre_shared_key_extension
            'extension_type::early_data': early_data_extension
            'extension_type::cookie': cookie_extension
            'extension_type::alpn': alpn_extension
    enums:
      extension_type:
        0x0000: server_name
        0x000a: supported_groups
        0x000d: signature_algorithms
        0x0010: alpn
        0x002b: supported_versions
        0x0033: key_share
        0x002d: psk_key_exchange_modes
        0x0029: pre_shared_key
        0x002a: early_data
        0x002c: cookie
  server_name_extension:
    seq:
      - id: server_name_list_length
        type: u2
      - id: server_name_list
        type: server_name_entry
        repeat: eos
  server_name_entry:
    seq:
      - id: name_type
        type: u1
      - id: name_length
        type: u2
      - id: name
        size: name_length
  supported_groups_extension:
    seq:
      - id: supported_groups_length
        type: u2
      - id: supported_groups
        type: u2
        enum: named_group
        repeat: expr
        repeat-expr: supported_groups_length / 2
  signature_algorithms_extension:
    seq:
      - id: supported_signature_algorithms_length
        type: u2
      - id: supported_signature_algorithms
        type: u2
        enum: signature_scheme
        repeat: expr
        repeat-expr: supported_signature_algorithms_length / 2
    enums:
      signature_scheme:
        0x0403: ecdsa_secp256r1_sha256
        0x0503: ecdsa_secp384r1_sha384
        0x0603: ecdsa_secp521r1_sha512
        0x0804: rsa_pss_rsae_sha256
        0x0805: rsa_pss_rsae_sha384
        0x0806: rsa_pss_rsae_sha512
  key_share_extension:
    seq:
      - id: client_shares_length
        type: u2
      - id: client_shares
        type: key_share_entry
        repeat: eos
  key_share_entry:
    seq:
      - id: group
        type: u2
        enum: named_group
      - id: key_exchange_length
        type: u2
      - id: key_exchange
        size: key_exchange_length
  supported_versions_extension:
    seq:
      - id: versions_length
        type: u1
      - id: versions
        type: u2
        repeat: expr
        repeat-expr: versions_length / 2
  psk_modes_extension:
    seq:
      - id: ke_modes_length
        type: u1
      - id: ke_modes
        type: u1
        enum: psk_key_exchange_mode
        repeat: expr
        repeat-expr: ke_modes_length
    enums:
      psk_key_exchange_mode:
        0: psk_ke
        1: psk_dhe_ke
  pre_shared_key_extension:
    seq:
      - id: identities_length
        type: u2
      - id: identities
        type: psk_identity
        repeat: eos
  psk_identity:
    seq:
      - id: identity_length
        type: u2
      - id: identity
        size: identity_length
      - id: obfuscated_ticket_age
        type: u4
  early_data_extension: {}
  cookie_extension:
    seq:
      - id: cookie_length
        type: u2
      - id: cookie
        size: cookie_length
  alpn_extension:
    seq:
      - id: alpn_list_length
        type: u2
      - id: alpn_list
        type: protocol_name
        repeat: eos
  protocol_name:
    seq:
      - id: name_length
        type: u1
      - id: name
        size: name_length