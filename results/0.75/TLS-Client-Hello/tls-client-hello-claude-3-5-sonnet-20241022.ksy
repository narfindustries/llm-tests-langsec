meta:
  id: tls_client_hello
  title: TLS Client Hello Parser
  file-extension: bin
  endian: be

seq:
  - id: record_header
    type: record_header
  - id: handshake_header
    type: handshake_header
  - id: client_version
    type: version
  - id: random
    type: random
  - id: session_id_length
    type: u1
  - id: session_id
    size: session_id_length
    if: session_id_length > 0
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
  record_header:
    seq:
      - id: content_type
        type: u1
      - id: version
        type: version
      - id: length
        type: u2

  handshake_header:
    seq:
      - id: msg_type
        type: u1
      - id: length
        type: b24

  version:
    seq:
      - id: major
        type: u1
      - id: minor
        type: u1

  random:
    seq:
      - id: gmt_unix_time
        type: u4
      - id: random_bytes
        size: 28

  cipher_suite:
    seq:
      - id: cipher_suite
        type: u2

  extension:
    seq:
      - id: type
        type: u2
      - id: length
        type: u2
      - id: data
        size: length

enums:
  content_type:
    20: change_cipher_spec
    21: alert
    22: handshake
    23: application_data

  handshake_type:
    1: client_hello
    2: server_hello
    11: certificate
    12: server_key_exchange
    14: server_hello_done
    16: client_key_exchange

  extension_type:
    0: server_name
    10: supported_groups
    11: ec_point_formats
    13: signature_algorithms
    16: application_layer_protocol_negotiation
    43: supported_versions
    45: psk_key_exchange_modes
    51: key_share