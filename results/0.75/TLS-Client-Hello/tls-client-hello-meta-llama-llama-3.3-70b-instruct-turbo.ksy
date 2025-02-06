meta:
  id: tls_client_hello
  endian: le

seq:
  - id: type
    type: u1
  - id: length
    type: u3
  - id: legacy_version
    type: u2
  - id: random
    type: bytes
    size: 32
  - id: legacy_session_id_length
    type: u1
  - id: legacy_session_id
    type: bytes
    size: legacy_session_id_length
  - id: cipher_suites
    type: tls_cipher_suite
    repeat: until
    until: (_pos + _size) >= _root._io.size
  - id: legacy_compression_methods
    type: u1
  - id: extensions
    type: tls_extension
    repeat: until
    until: (_pos + _size) >= _root._io.size

types:
  tls_cipher_suite:
    seq:
      - id: value
        type: u2

  tls_extension:
    seq:
      - id: extension_type
        type: u2
      - id: extension_length
        type: u2
      - id: extension_data
        type:
          switch_on: extension_type
          cases:
            '0x0000': # server_name
              seq:
                - id: name_type
                  type: u1
                - id: name
                  type: bytes
                  size: expr
                  size_expr: extension_length - 1
            '0x0001': # max_fragment_length
              seq:
                - id: max_fragment_length
                  type: u1
            '0x0002': # client_certificate_url
              seq:
                - id: url_length
                  type: u2
                - id: url
                  type: bytes
                  size: expr
                  size_expr: url_length
            '0x0003': # trusted_ca_keys
              seq:
                - id: trusted_authorities
                  type: trusted_authority
                  repeat: until
                  until: (_pos + _size) >= _root._io.size
            '0x0004': # truncated_hmac
              seq: []
            '0x0005': # status_request
              seq:
                - id: certificate_status_type
                  type: u1
                - id: responder_id_list_length
                  type: u2
                - id: responder_id_list
                  type: bytes
                  size: expr
                  size_expr: responder_id_list_length
                - id: extensions
                  type: tls_extension
                  repeat: until
                  until: (_pos + _size) >= _root._io.size
            '0x000d': # signature_algorithms
              seq:
                - id: supported_signature_algorithms
                  type: supported_signature_algorithm
                  repeat: until
                  until: (_pos + _size) >= _root._io.size
            '0x0010': # alpn
              seq:
                - id: supported_protocols
                  type: bytes
                  size: expr
                  size_expr: extension_length
            '0x0012': # signed_certificate_timestamp
              seq:
                - id: scts
                  type: sct
                  repeat: until
                  until: (_pos + _size) >= _root._io.size
            '0x0013': # client_certificate_type
              seq:
                - id: certificate_types
                  type: u1
                  repeat: until
                  until: (_pos + _size) >= _root._io.size
            '0x0014': # server_certificate_type
              seq:
                - id: certificate_types
                  type: u1
                  repeat: until
                  until: (_pos + _size) >= _root._io.size
            '0x0015': # padding
              seq: []
            '0x0016': # key_share
              seq:
                - id: client_shares
                  type: client_share
                  repeat: until
                  until: (_pos + _size) >= _root._io.size
            '0x0017': # pre_shared_key
              seq:
                - id: identities
                  type: psk_identity
                  repeat: until
                  until: (_pos + _size) >= _root._io.size
            '0x0018': # early_data
              seq:
                - id: max_early_data_size
                  type: u1
            '0x0019': # supported_versions
              seq:
                - id: versions
                  type: u2
                  repeat: until
                  until: (_pos + _size) >= _root._io.size
            '0x002b': # cookie
              seq:
                - id: cookie
                  type: bytes
                  size: expr
                  size_expr: extension_length
            '0x002d': # psk_key_exchange_modes
              seq:
                - id: ke_modes
                  type: u1
                  repeat: until
                  until: (_pos + _size) >= _root._io.size
            '0x0032': # ticket_early_data_info
              seq:
                - id: max_early_data_size
                  type: u1
            '0x0035': # supported_versions (also used for HelloRetryRequest)
              seq:
                - id: versions
                  type: u2
                  repeat: until
                  until: (_pos + _size) >= _root._io.size

  trusted_authority:
    seq:
      - id: identifier_type
        type: u1
      - id: identifier
        type: bytes
        size: expr
        size_expr: extension_length - 1

  supported_signature_algorithm:
    seq:
      - id: signature_hash_algorithm_pair
        type: u2

  sct:
    seq:
      - id: sct_version
        type: u2
      - id: log_id
        type: bytes
        size: expr
        size_expr: extension_length - 10
      - id: timestamp
        type: u8
      - id: extensions
        type: tls_extension
        repeat: until
        until: (_pos + _size) >= _root._io.size
      - id: signature
        type: bytes
        size: expr
        size_expr: extension_length - 14

  client_share:
    seq:
      - id: group
        type: u2
      - id: key_exchange
        type: bytes
        size: expr
        size_expr: extension_length - 2

  psk_identity:
    seq:
      - id: identity
        type: bytes
        size: expr
        size_expr: extension_length - 2
      - id: obfuscated_ticket_age
        type: u2