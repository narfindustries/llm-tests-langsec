domain BitcoinTransactions {
  doc "Bitcoin transactions file format"

  type BitcoinTransaction {
    doc "A single bitcoin transaction"

    field transaction_id: uint32little
    field version: uint32little
    field num_inputs: varint
    field inputs: array[Input](num_inputs)
    field num_outputs: varint
    field outputs: array[Output](num_outputs)
    field locktime: uint32little
  }

  type Input {
    doc "A single input in a bitcoin transaction"

    field previous_output_hash: bytes(32)
    field previous_output_index: uint32little
    field script_length: varint
    field script: bytes(script_length)
    field sequence: uint32little
  }

  type Output {
    doc "A single output in a bitcoin transaction"

    field value: int64little
    field script_length: varint
    field script: bytes(script_length)
  }

  type varint {
    doc "A variable-length integer"

    expression
      if (value < 0xfd) {
        uint8(value)
      } else if (value < 0xffff) {
        uint8(0xfd) + uint16little(value)
      } else if (value < 0xffffffff) {
        uint8(0xfe) + uint32little(value)
      } else {
        uint8(0xff) + uint64little(value)
      }
  }

  root_type BitcoinTransaction
}

domain Error {
  doc "Error handling"

  type Error {
    doc "An error"

    field message: string
  }

  root_type Error
}