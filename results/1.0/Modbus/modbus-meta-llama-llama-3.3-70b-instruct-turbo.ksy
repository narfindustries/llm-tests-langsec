meta:
  id: modbus
  title: Modbus
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
    type:
      switch-on: function_code
      cases:
        1: data_fc1
        2: data_fc2
        3: data_fc3
        4: data_fc4
        5: data_fc5
        6: data_fc6
        15: data_fc15
        16: data_fc16
        ':default': data_default
types:
  data_fc1:
    seq:
      - id: coil_status
        type: bits
        size: 8
  data_fc2:
    seq:
      - id: input_status
        type: bits
        size: 8
  data_fc3:
    seq:
      - id: register_values
        type: u2
        repeat: expr
        repeat-expr: (length - 3) // 2
  data_fc4:
    seq:
      - id: input_register_values
        type: u2
        repeat: expr
        repeat-expr: (length - 3) // 2
  data_fc5:
    seq:
      - id: coil_address
        type: u2
      - id: coil_value
        type: u2
  data_fc6:
    seq:
      - id: register_address
        type: u2
      - id: register_value
        type: u2
  data_fc15:
    seq:
      - id: coil_addresses
        type: u2
        repeat: expr
        repeat-expr: (length - 3) // 2
      - id: coil_values
        type: bits
        size: expr
        size-expr: (length - 3) * 8
  data_fc16:
    seq:
      - id: register_addresses
        type: u2
        repeat: expr
        repeat-expr: (length - 3) // 2
      - id: register_values
        type: u2
        repeat: expr
        repeat-expr: (length - 3) // 2
  data_default:
    seq:
      - id: data_bytes
        type: u1
        repeat: expr
        repeat-expr: length - 3