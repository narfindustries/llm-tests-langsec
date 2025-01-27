meta:
  id: bitcoin_transactions
  file-extension: btc
  endian: little

doc: |
  This is a Kaitai Struct specification to parse Bitcoin transaction files.

seq:
  - id: version
    type: u4
    doc: Transaction version number.

  - id: input_count
    type: varint
    doc: Number of transaction inputs.

  - id: inputs
    type: input
    repeat: expr
    repeat-expr: input_count
    doc: Transaction inputs.

  - id: output_count
    type: varint
    doc: Number of transaction outputs.

  - id: outputs
    type: output
    repeat: expr
    repeat-expr: output_count
    doc: Transaction outputs.

  - id: lock_time
    type: u4
    doc: Transaction lock time.

types:
  varint:
    doc: |
      Variable-length integer used in Bitcoin protocol.
    seq:
      - id: value
        type:
          switch-on: first_byte
          cases:
            '0xfd': u2
            '0xfe': u4
            '0xff': u8
            _: u1
      - id: first_byte
        type: u1

  input:
    seq:
      - id: previous_output
        type: outpoint
        doc: Previous output transaction reference.

      - id: script_length
        type: varint
        doc: Length of the signature script.

      - id: script_sig
        size: script_length
        doc: Signature script.

      - id: sequence
        type: u4
        doc: Transaction input sequence number.

  output:
    seq:
      - id: value
        type: u8
        doc: Transaction output value in satoshis.

      - id: script_length
        type: varint
        doc: Length of the public key script.

      - id: script_pub_key
        size: script_length
        doc: Public key script.

  outpoint:
    seq:
      - id: tx_hash
        type: bytes
        size: 32
        doc: Previous transaction hash.

      - id: index
        type: u4
        doc: Previous output index.