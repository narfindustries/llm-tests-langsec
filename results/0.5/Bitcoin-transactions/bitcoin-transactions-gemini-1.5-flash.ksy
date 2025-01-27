meta:
  id: bitcoin-transactions-gemini-1
  title: Bitcoin Transactions (Gemini 1)
  homepage: https://github.com/kaitai-io/kaitai_struct_formats
  license: Apache-2.0
  doc_url: https://kaitai.io/formats/bitcoin-transactions-gemini-1
  experimental: true

types:
  transaction:
    seq:
      - id: version
        type: u4le
      - id: num_inputs
        type: u4le
      - id: inputs
        type: seq
        repeat: expr
        repeat-expr: num_inputs
        contents:
          - id: prev_tx_hash
            type: bytes
            size: 32
          - id: prev_tx_index
            type: u4le
          - id: script_sig
            type: bytes
            size: 1
            read: lambda x: x.read_bytes(x.script_sig_len)
          - id: script_sig_len
            type: u4le
          - id: sequence
            type: u4le
      - id: num_outputs
        type: u4le
      - id: outputs
        type: seq
        repeat: expr
        repeat-expr: num_outputs
        contents:
          - id: value
            type: u8le
          - id: script_pubkey
            type: bytes
            size: 1
            read: lambda x: x.read_bytes(x.script_pubkey_len)
          - id: script_pubkey_len
            type: u4le
      - id: lock_time
        type: u4le

