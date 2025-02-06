meta:
  id: tls_client_hello
  title: TLS Client Hello Message
  endian: be
seq:
  - id: legacy_version
    type: u2
  - id: random
    size: 32
  - id: num_legacy_session_id
    type: u1
  - id: legacy_session_id
    type: u1
    repeat: expr
    repeat-expr: num_legacy_session_id
  - id: cipher_suites_length
    type: u2
  - id: cipher_suites
    type: u2
    repeat: expr
    repeat-expr: cipher_suites_length / 2
  - id: num_legacy_compression_methods
    type: u1
  - id: legacy_compression_methods
    type: u1
    repeat: expr
    repeat-expr: num_legacy_compression_methods
  - id: num_extensions
    type: u2
  - id: extensions
    type: tls_extension
    repeat: expr
    repeat-expr: num_extensions

types:
  tls_extension:
    seq:
      - id: extension_type
        type: u2
      - id: len_extension_data
        type: u2
      - id: extension_data
        size: len_extension_data
    instances:
      parsed:
        pos: 0
        io: extension_data._io
        type:
          switch-on: extension_type
          cases:
            0: server_name_list
            1: max_fragment_length
            5: status_request
            10: supported_groups
            13: signature_algorithms
            14: use_srtp
            15: heartbeat
            16: application_layer_protocol_negotiation
            43: supported_versions
            44: cookie
            45: psk_key_exchange_modes
            47: certificate_authorities
            48: oid_filters
            49: post_handshake_auth
            50: signature_algorithms_cert
            51: key_share

  server_name_list:
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
      - id: name_length
        type: u2
      - id: name
        type: str
        encoding: UTF-8
        size: name_length

  max_fragment_length:
    seq:
      - id: fragment_size
        type: u1

  status_request:
    seq:
      - id: status_type
        type: u1
      - id: len_responder_id_list
        type: u2
      - id: responder_id_list
        size: len_responder_id_list
      - id: len_request_extensions
        type: u2
      - id: request_extensions
        size: len_request_extensions

  supported_groups:
    seq:
      - id: list_length
        type: u2
      - id: groups
        type: u2
        repeat: expr
        repeat-expr: list_length / 2

  signature_algorithms:
    seq:
      - id: algorithms_length
        type: u2
      - id: algorithms
        type: u2
        repeat: expr
        repeat-expr: algorithms_length / 2

  use_srtp:
    seq:
      - id: protection_profiles_length
        type: u2
      - id: protection_profiles
        type: u2
        repeat: expr
        repeat-expr: protection_profiles_length / 2
      - id: len_mki
        type: u1
      - id: mki
        size: len_mki

  heartbeat:
    seq:
      - id: mode
        type: u1

  application_layer_protocol_negotiation:
    seq:
      - id: num_alpn_strings
        type: u2
      - id: alpn_strings
        type: alpn_string
        repeat: expr
        repeat-expr: num_alpn_strings

  alpn_string:
    seq:
      - id: str_length
        type: u1
      - id: str
        type: str
        encoding: UTF-8
        size: str_length

  supported_versions:
    seq:
      - id: versions_length
        type: u2
      - id: versions
        type: u2
        repeat: expr
        repeat-expr: versions_length / 2

  cookie:
    seq:
      - id: len_cookie
        type: u2
      - id: cookie
        size: len_cookie

  psk_key_exchange_modes:
    seq:
      - id: num_ke_modes
        type: u1
      - id: ke_modes
        type: u1
        repeat: expr
        repeat-expr: num_ke_modes

  certificate_authorities:
    seq:
      - id: num_authorities
        type: u2
      - id: authorities
        type: distinguished_name
        repeat: expr
        repeat-expr: num_authorities

  distinguished_name:
    seq:
      - id: len_name
        type: u2
      - id: name
        size: len_name

  oid_filters:
    seq:
      - id: num_filters
        type: u2
      - id: filters
        type: oid_filter
        repeat: expr
        repeat-expr: num_filters

  oid_filter:
    seq:
      - id: len_ca
        type: u2
      - id: ca
        size: len_ca
      - id: len_oid
        type: u2
      - id: oid
        size: len_oid

  post_handshake_auth:
    seq:
      - id: empty
        type: u1

  signature_algorithms_cert:
    seq:
      - id: algorithms_length
        type: u2
      - id: algorithms
        type: u2
        repeat: expr
        repeat-expr: algorithms_length / 2

  key_share:
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