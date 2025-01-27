meta:
  id: modbus
  endian: big
  file-extension: modbus
  license: MIT
  ks-version: 0.9

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
    type: modbus_data
    size: length - 2

types:
  modbus_data:
    seq:
      - id: coils
        type: u1
        if: function_code == 1 or function_code == 15
      - id: discrete_inputs
        type: u1
        if: function_code == 2
      - id: holding_registers
        type: u2
        if: function_code == 3 or function_code == 16
      - id: input_registers
        type: u2
        if: function_code == 4