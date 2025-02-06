meta:
  id: bitcoin_transaction
  title: Bitcoin Transaction
  endian: le
doc: |
  A Bitcoin transaction specifies how Bitcoins are transferred, who from, and to whom. It includes a list of inputs and outputs as well as a locktime.

seq:
  - id: version
    type: u4
    doc: Transaction version number.

  - id: input_count
    type: vlq_base128_le
    doc: Number of input transactions.

  - id: inputs
    type: input
    repeat: expr
    repeat-expr: input_count.value
    doc: List of inputs.

  - id: output_count
    type: vlq_base128_le
    doc: Number of outputs.

  - id: outputs
    type: output
    repeat: expr
    repeat-expr: output_count.value
    doc: List of outputs.

  - id: locktime
    type: u4
    doc: The block number or timestamp at which this transaction is locked.

types:
  input:
    seq:
      - id: prev_transaction_hash
        type: u8
        repeat: expr
        repeat-expr: 4
        doc: The hash of the transaction from which the inputs are sourced.

      - id: output_index
        type: u4
        doc: The index of the output in the previous transaction being spent.

      - id: script_length
        type: vlq_base128_le
        doc: Length of the ScriptSig.

      - id: script_sig
        type: u1
        repeat: expr
        repeat-expr: script_length.value
        doc: Script providing the conditions to redeem the previous output.

      - id: sequence
        type: u4
        doc: Sequence number, default is 0xFFFFFFFF.

  output:
    seq:
      - id: value
        type: u8
        doc: Amount of satoshis to transfer.

      - id: script_length
        type: vlq_base128_le
        doc: Length of the ScriptPubKey.

      - id: script_pub_key
        type: u1
        repeat: expr
        repeat-expr: script_length.value
        doc: Script specifying the conditions under which this output can be claimed.

  vlq_base128_le:
    seq:
      - id: groups
        type: u1
        repeat: until
        repeat-until: _.value & 0x80 == 0
        doc: Base128 length encoding, little endian, least significant group first.
    instances:
      value:
        value: '_.groups.map((g, i) => (g & 0x7f) << (7 * i)).reduce((a, b) => a + b, 0)'
        doc: Decode the variable length quantity from the groups.