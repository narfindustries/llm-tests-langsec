meta:
  id: tls_client_hello
  title: TLS 1.3 Client Hello Message
  file-extension: bin
  endian: be

seq:
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

  extension:
    seq:
      - id: extension_type
        type: u2
        enum: extension_type_enum
      - id: extension_length
        type: u2
      - id: extension_data
        size: extension_length
        type:
          switch-on: extension_type
          cases:
            'extension_type_enum::server_name': server_name_extension
            'extension_type_enum::supported_groups': supported_groups_extension
            'extension_type_enum::signature_algorithms': signature_algorithms_extension
            'extension_type_enum::key_share': key_share_extension
            'extension_type_enum::supported_versions': supported_versions_extension
            'extension_type_enum::psk_key_exchange_modes': psk_modes_extension
            'extension_type_enum::pre_shared_key': pre_shared_key_extension
            _: raw_extension

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
        enum: supported_groups_enum
        repeat: expr
        repeat-expr: supported_groups_length / 2

  signature_algorithms_extension:
    seq:
      - id: supported_signature_algorithms_length
        type: u2
      - id: supported_signature_algorithms
        type: u2
        enum: signature_algorithm_enum
        repeat: expr
        repeat-expr: supported_signature_algorithms_length / 2

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
        enum: supported_groups_enum
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
      - id: psk_modes_length
        type: u1
      - id: psk_modes
        type: u1
        enum: psk_key_exchange_mode_enum
        repeat: expr
        repeat-expr: psk_modes_length

  pre_shared_key_extension:
    seq:
      - id: identities_length
        type: u2
      - id: identities
        type: psk_identity
        repeat: eos
      - id: binders_length
        type: u2
      - id: binders
        type: psk_binder
        repeat: eos

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

  raw_extension:
    seq:
      - id: data
        size-eos: true

enums:
  cipher_suite_enum:
    0x1301: tls_aes_128_gcm_sha256
    0x1302: tls_aes_256_gcm_sha384
    0x1303: tls_chacha20_poly1305_sha256

  extension_type_enum:
    0x0000: server_name
    0x000a: supported_groups
    0x000d: signature_algorithms
    0x0015: padding
    0x0029: pre_shared_key
    0x002a: early_data
    0x002b: supported_versions
    0x002c: cookie
    0x002d: psk_key_exchange_modes
    0x0033: key_share

  supported_groups_enum:
    0x0017: secp256r1
    0x0018: secp384r1
    0x0019: secp521r1
    0x001d: x25519
    0x001e: x448

  signature_algorithm_enum:
    0x0403: ecdsa_secp256r1_sha256
    0x0503: ecdsa_secp384r1_sha384
    0x0603: ecdsa_secp521r1_sha512
    0x0804: rsa_pss_rsae_sha256
    0x0805: rsa_pss_rsae_sha384
    0x0806: rsa_pss_rsae_sha512

  psk_key_exchange_mode_enum:
    0: psk_ke
    1: psk_dhe_ke