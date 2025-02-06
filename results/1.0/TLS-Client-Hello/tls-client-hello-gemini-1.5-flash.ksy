types:
  client_hello:
    seq:
      - id: client_version
        type: u2be
      - id: random
        type: u1
        size: 32
      - id: legacy_session_id
        type: u1
        size: 32
      - id: cipher_suites
        type: u2be
        repeat: eos
      - id: compressed_certificate_types
        type: u1
        repeat: eos
      - id: supported_versions
        type: u2be
        repeat: eos
      - id: supported_groups
        type: u2be
        repeat: eos
      - id: extensions
        type: extension
        repeat: eos
      - id: signature_algorithms_cert
        type: u2be
        repeat: eos
      - id: signature_algorithms_psk
        type: u2be
        repeat: eos

  extension:
    seq:
      - id: extension_type
        type: u2be
      - id: extension_length
        type: u2be
      - id: extension_data
        type: u1
        size: extension_length

