meta:
  id: modbus
  title: Modbus Protocol
  file-extension: modbus
  endian: be

seq:
  - id: transaction_id
    type: u2
    doc: Transaction Identifier (Modbus TCP only)

  - id: protocol_id
    type: u2
    doc: Protocol Identifier (Modbus TCP only, always 0x0000)

  - id: length
    type: u2
    doc: Length of the remaining bytes in the message (Modbus TCP only)

  - id: unit_id
    type: u1
    doc: Unit Identifier (Modbus TCP only)

  - id: address
    type: u1
    doc: Address of the slave device (Modbus RTU/ASCII)

  - id: function_code
    type: u1
    doc: Function code defining the operation

  - id: data
    size: length - 2
    type: data_block
    doc: Data block containing function-specific information

  - id: error_check
    type: u2
    doc: Error checking field (CRC for RTU, LRC for ASCII)

types:
  data_block:
    seq:
      - id: start_address
        type: u2
        doc: Starting address for read/write operations

      - id: quantity
        type: u2
        doc: Quantity of items to read/write

      - id: values
        size-eos: true
        doc: Values to write (only for write operations)