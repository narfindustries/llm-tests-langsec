meta:
  id: tls_client_hello
  title: TLS Protocol Client Hello Message
  license: CC0-1.0
  endian: be

seq:
  - id: legacy_version
    type: u2
  - id: random
    size: 32
  - id: session_id_length
    type: u1
  - id: session_id
    size: session_id_length
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
    type: tls_extension
    repeat: until
    repeat-until: _.length == 0
    size-eos: true

types:
  cipher_suite:
    seq:
      - id: identifier
        type: u2

  tls_extension:
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
            0: server_name # Server Name Indication
            1: generic_extension # Placeholder for max_fragment_length
            2: generic_extension # Placeholder for client_certificate_url
            5: generic_extension # Placeholder for status_request
            10: supported_groups
            11: generic_extension # Placeholder for ec_point_formats
            13: signature_algorithms
            16: generic_extension # Placeholder for application_layer_protocol_negotiation
            18: generic_extension # Placeholder for signed_certificate_timestamp
            21: generic_extension # Placeholder for padding
            22: generic_extension # Placeholder for pre_shared_key
            23: generic_extension # Placeholder for early_data
            24: supported_versions
            25: generic_extension # Placeholder for cookie
            26: generic_extension # Placeholder for psk_key_exchange_modes
            27: generic_extension # Placeholder for certificate_authorities
            28: generic_extension # Placeholder for oid_filters
            29: generic_extension # Placeholder for post_handshake_auth
            31: key_share

  generic_extension:
    seq:
      - id: contents
        type: u1
        repeat: expr
        repeat-expr: _parent.length

  server_name:
    seq:
      - id: list_length
        type: u2
      - id: server_name_list
        type: server_name_type
        repeat: expr
        repeat-expr: list_length / 5 # Assuming single name type and 5 bytes per entry

  server_name_type:
    seq:
      - id: name_type
        type: u1
      - id: name_length
        type: u2
      - id: server_name
        type: str
        size: name_length
        encoding: UTF-8

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
      - id: list_length
        type: u2
      - id: algorithms
        type: u2
        repeat: expr
        repeat-expr: list_length / 2

  supported_versions:
    seq:
      - id: list_length
        type: u1
      - id: versions
        type: u2
        repeat: expr
        repeat-expr: list_length / 2

  key_share:
    seq:
      - id: list_length
        type: u2
      - id: key_exchanges
        type: key_exchange_entry
        repeat: expr
        repeat-expr: list_length / 4

  key_exchange_entry:
    seq:
      - id: group
        type: u2
      - id: key_exchange_length
        type: u2
      - id: key_exchange
        size: key_exchange_length