meta:
  id: tls_client_hello
  endian: be
seq:
  - id: legacy_version
    type: u2
    doc: Always set to 0x0303 (TLS 1.2)
  
  - id: random
    size: 32
    doc: 32 bytes of cryptographically secure random data
  
  - id: legacy_session_id_length
    type: u1
  
  - id: legacy_session_id
    size: legacy_session_id_length
    doc: Optional session ID (0-32 bytes)
  
  - id: cipher_suites_length
    type: u2
  
  - id: cipher_suites
    type: u2
    repeat: expr
    repeat-expr: cipher_suites_length / 2
    doc: List of supported cipher suites
  
  - id: legacy_compression_methods_length
    type: u1
  
  - id: legacy_compression_methods
    size: legacy_compression_methods_length
    doc: Always contains [0] (null compression)
  
  - id: extensions_length
    type: u2
  
  - id: extensions
    type: extension
    repeat: expr
    repeat-expr: extensions.size
    doc: TLS extensions

types:
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
            'extension_type::server_name_type': server_name_ext
            'extension_type::supported_versions': supported_versions_ext
            'extension_type::signature_algorithms': signature_algorithms_ext
            'extension_type::supported_groups': supported_groups_ext
            'extension_type::key_share': key_share_ext
            'extension_type::pre_shared_key': pre_shared_key_ext
            'extension_type::psk_key_exchange_modes': psk_key_exchange_modes_ext
            'extension_type::early_data': early_data_ext
            '_': raw_extension

  server_name_ext:
    seq:
      - id: num_server_names
        type: u2
      - id: server_names
        type: server_name
        repeat: expr
        repeat-expr: num_server_names

  server_name:
    seq:
      - id: name_type
        type: u1
      - id: len_hostname
        type: u2
      - id: hostname
        size: len_hostname

  supported_versions_ext:
    seq:
      - id: versions_length
        type: u1
      - id: versions
        type: u2
        repeat: expr
        repeat-expr: versions_length / 2

  signature_algorithms_ext:
    seq:
      - id: algorithms_length
        type: u2
      - id: algorithms
        type: u2
        repeat: expr
        repeat-expr: algorithms_length / 2

  supported_groups_ext:
    seq:
      - id: groups_length
        type: u2
      - id: groups
        type: u2
        repeat: expr
        repeat-expr: groups_length / 2

  key_share_ext:
    seq:
      - id: num_client_shares
        type: u2
      - id: client_shares
        type: key_share_entry
        repeat: expr
        repeat-expr: num_client_shares

  key_share_entry:
    seq:
      - id: group
        type: u2
      - id: len_key_exchange
        type: u2
      - id: key_exchange
        size: len_key_exchange

  pre_shared_key_ext:
    seq:
      - id: num_identities
        type: u2
      - id: identities
        type: psk_identity
        repeat: expr
        repeat-expr: num_identities
      - id: num_binders
        type: u2
      - id: binders
        type: opaque_vector
        repeat: expr
        repeat-expr: num_binders

  psk_identity:
    seq:
      - id: len_identity
        type: u2
      - id: identity
        size: len_identity
      - id: obfuscated_ticket_age
        type: u4

  psk_key_exchange_modes_ext:
    seq:
      - id: num_modes
        type: u1
      - id: modes
        type: u1
        repeat: expr
        repeat-expr: num_modes

  early_data_ext:
    seq: []

  raw_extension:
    seq:
      - id: raw_data
        size-eos: true

  opaque_vector:
    seq:
      - id: len_data
        type: u2
      - id: data
        size: len_data

enums:
  extension_type:
    0: server_name_type
    10: supported_groups
    11: ec_point_formats
    13: signature_algorithms
    16: alpn
    18: signed_certificate_timestamp
    23: extended_master_secret
    33: key_share
    41: pre_shared_key
    42: early_data
    43: supported_versions
    44: cookie
    45: psk_key_exchange_modes
    65281: renegotiation_info