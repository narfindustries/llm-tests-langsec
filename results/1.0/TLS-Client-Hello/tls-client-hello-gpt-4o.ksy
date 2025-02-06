meta:
  id: tls_client_hello
  title: TLS Client Hello
  application: tls
  file-extension: bin
  endian: be

seq:
  - id: legacy_version
    type: u2

  - id: random
    size: 32

  - id: legacy_session_id
    type: session_id

  - id: cipher_suites_length
    type: u2

  - id: cipher_suites
    size: cipher_suites_length

  - id: legacy_compression_methods_length
    type: u1

  - id: legacy_compression_methods
    size: legacy_compression_methods_length

  - id: extensions_length
    type: u2

  - id: extensions
    type: extensions
    size: extensions_length

types:
  session_id:
    seq:
      - id: length
        type: u1
      - id: session_id
        size: length

  extensions:
    seq:
      - id: extension_list
        type: extension
        repeat: eos

  extension:
    seq:
      - id: type
        type: u2
      - id: length
        type: u2
      - id: data
        size: length

enums:
  cipher_suites:
    0x1301: tls_aes_128_gcm_sha256
    0x1302: tls_aes_256_gcm_sha384
    0x1303: tls_chacha20_poly1305_sha256

  legacy_compression_methods:
    0x00: no_compression

  extensions_type:
    0x0000: server_name
    0x000a: supported_groups
    0x0010: application_layer_protocol_negotiation
    0x002b: supported_versions
    0x0033: key_share
    0x003d: psk_key_exchange_modes
    0x0023: pre_shared_key