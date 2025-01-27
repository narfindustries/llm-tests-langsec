meta:
  id: modbus
  title: Modbus Protocol
  endian: big

seq:
  - id: transaction_id
    type: u2
  - id: protocol_id
    type: u2
  - id: length
    type: u2
  - id: unit_id
    type: u1
  - id: function_code
    type: u1
  - id: data
    size: length - 2
    type: data_block

types:
  data_block:
    seq:
      - id: coil_status
        type: u1
        if: _parent.function_code == 1
      - id: input_status
        type: u1
        if: _parent.function_code == 2
      - id: holding_registers
        type: u2
        if: _parent.function_code == 3
      - id: input_registers
        type: u2
        if: _parent.function_code == 4
      - id: exception_code
        type: u1
        if: _parent.function_code >= 128