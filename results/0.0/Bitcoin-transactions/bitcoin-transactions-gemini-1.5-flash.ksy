meta:
  id: bitcoin-transactions-gemini-1
  title: Bitcoin Transactions (Gemini 1.5 Flash)
  homepage: https://github.com/kaitai-io/kaitai_struct_formats
  file-extension: ksy
  license: Apache-2.0

types:
  transaction:
    seq:
      - id: version
        type: u4le
      - id: input_count
        type: u4le
      - id: inputs
        type: seq
        repeat: expr
        expr: input_count
        contents: input
      - id: output_count
        type: u4le
      - id: outputs
        type: seq
        repeat: expr
        expr: output_count
        contents: output
      - id: lock_time
        type: u4le

  input:
    seq:
      - id: prev_tx_hash
        type: str
        size: 32
      - id: prev_tx_index
        type: u4le
      - id: script_sig
        type: bytes
        size: lambda: script_sig_len
      - id: sequence
        type: u4le

  output:
    seq:
      - id: value
        type: u8le
      - id: script_pubkey
        type: bytes
        size: lambda: script_pubkey_len

  script_sig_len:
    expr: (self.script_sig_len_bytes).read_u4le()

  script_pubkey_len:
    expr: (self.script_pubkey_len_bytes).read_u4le()

  script_sig_len_bytes:
    expr: self.script_sig

  script_pubkey_len_bytes:
    expr: self.script_pubkey

