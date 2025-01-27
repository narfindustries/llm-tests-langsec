meta:
  id: tls_client_hello
  title: TLS Client Hello Message
  license: MIT
  ks-version: 0.9
  endian: be
  imports:
    - tls_record_layer

seq:
  - id: record
    type: tls_record_layer.tls_record
  - id: handshake_type
    type: u1
    enum: handshake_types
  - id: length
    type: u3
  - id: version
    type: u2
    enum: tls_versions
  - id: random
    size: 32
  - id: session_id_length
    type: u1
  - id: session_id
    size: session_id_length
  - id: cipher_suites_length
    type: u2
  - id: cipher_suites
    size: cipher_suites_length
    type: u2
    repeat: expr
    repeat-expr: cipher_suites_length / 2
  - id: compression_methods_length
    type: u1
  - id: compression_methods
    size: compression_methods_length
    type: u1
    repeat: expr
    repeat-expr: compression_methods_length
  - id: extensions_length
    type: u2
  - id: extensions
    size: extensions_length
    type: tls_extension
    repeat: expr
    repeat-expr: extensions_length

types:
  tls_extension:
    seq:
      - id: type
        type: u2
        enum: extension_types
      - id: length
        type: u2
      - id: data
        size: length

enums:
  handshake_types:
    1: client_hello
  tls_versions:
    0x0301: tls_1_0
    0x0302: tls_1_1
    0x0303: tls_1_2
    0x0304: tls_1_3
  extension_types:
    0x0000: server_name
    0x0005: status_request
    0x000a: supported_groups
    0x000b: ec_point_formats
    0x000d: signature_algorithms
    0x0010: application_layer_protocol_negotiation
    0x0017: extended_master_secret
    0x0023: session_ticket
    0x002b: supported_versions
    0x002d: psk_key_exchange_modes
    0x0033: key_share
    0x0035: pre_shared_key