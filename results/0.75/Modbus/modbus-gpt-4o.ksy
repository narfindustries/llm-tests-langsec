meta:
  id: modbus
  title: Modbus Protocol
  endian: be

seq:
  - id: transaction_id
    type: u2
    doc: Transaction Identifier

  - id: protocol_id
    type: u2
    doc: Protocol Identifier

  - id: length
    type: u2
    doc: Length of remaining bytes

  - id: unit_id
    type: u1
    doc: Unit Identifier

  - id: function_code
    type: u1
    doc: Function Code

  - id: data
    size: length - 2
    doc: Data, varies based on function code

types:
  modbus_rtu:
    seq:
      - id: unit_id
        type: u1
        doc: Unit Identifier

      - id: function_code
        type: u1
        doc: Function Code

      - id: data
        size-eos: true
        doc: Data, varies based on function code

      - id: crc
        type: u2
        doc: CRC for error checking

  modbus_ascii:
    seq:
      - id: start_char
        type: u1
        doc: Start character, typically ':'

      - id: unit_id
        type: str
        encoding: ascii
        size: 2
        doc: Unit Identifier in ASCII hex

      - id: function_code
        type: str
        encoding: ascii
        size: 2
        doc: Function Code in ASCII hex

      - id: data
        type: str
        encoding: ascii
        size-eos: true
        doc: Data in ASCII hex, varies based on function code

      - id: lrc
        type: str
        encoding: ascii
        size: 2
        doc: LRC for error checking in ASCII hex

enums:
  function_codes:
    1: read_coils
    2: read_discrete_inputs
    3: read_holding_registers
    4: read_input_registers
    5: write_single_coil
    6: write_single_register
    15: write_multiple_coils
    16: write_multiple_registers