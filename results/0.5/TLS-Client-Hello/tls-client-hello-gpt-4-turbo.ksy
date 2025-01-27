meta:
  id: tls_client_hello
  title: TLS Client Hello
  file-extension: dat
  endian: be
  license: CC0-1.0
  ks-version: 0.9

doc: |
  The TLS Client Hello message, initiating a TLS handshake.

seq:
  - id: content_type
    type: u1
    enum: content_type
  - id: version
    type: tls_version
  - id: length
    type: u2
  - id: handshake
    type: handshake

types:
  tls_version:
    seq:
      - id: major
        type: u1
      - id: minor
        type: u1

  handshake:
    seq:
      - id: handshake_type
        type: u1
        enum: handshake_type
      - id: length
        type: u3
      - id: client_hello
        type: client_hello
        if: handshake_type == handshake_type::client_hello

  client_hello:
    seq:
      - id: version
        type: tls_version
      - id: random
        size: 32
      - id: session_id_length
        type: u1
      - id: session_id
        size: session_id_length
      - id: cipher_suites_length
        type: u2
      - id: cipher_suites
        type: u2
        repeat: expr
        repeat-expr: cipher_suites_length / 2
      - id: compression_methods_length
        type: u1
      - id: compression_methods
        type: u1
        repeat: expr
        repeat-expr: compression_methods_length
      - id: extensions_length
        type: u2
        if: _io.pos + _io.size - 2 > _io.pos
      - id: extensions
        type: extension
        repeat: eos
        if: extensions_length > 0

  extension:
    seq:
      - id: type
        type: u2
        enum: extension_type
      - id: length
        type: u2
      - id: data
        size: length

enums:
  content_type:
    0x16: handshake

  handshake_type:
    0x01: client_hello

  extension_type:
    0x0000: server_name
    0x000d: signature_algorithms
    0x0010: supported_groups
    0x0017: extended_master_secret
    0x002b: supported_versions
    0x002d: psk_key_exchange_modes
    0x0033: key_share