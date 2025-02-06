meta:
  id: modbus_frame
  title: Modbus Application Protocol
  file-extension: mbap
  endian: be
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
        1: coils_status
        2: input_status
        3: holding_register
        4: input_register
        5: single_coil
        6: single_register
        15: multiple_coils
        16: multiple_registers
types:
  coils_status:
    seq:
      - id: status
        type: b1
        repeat: expr
        repeat-expr: (_parent.length - 1) * 8
  input_status:
    seq:
      - id: status
        type: b1
        repeat: expr
        repeat-expr: (_parent.length - 1) * 8
  holding_register:
    seq:
      - id: values
        type: u2
        repeat: expr
        repeat-expr: (_parent.length - 1) / 2
  input_register:
    seq:
      - id: values
        type: u2
        repeat: expr
        repeat-expr: (_parent.length - 1) / 2
  single_coil:
    seq:
      - id: output_address
        type: u2
      - id: output_value
        type: u2
  single_register:
    seq:
      - id: register_address
        type: u2
      - id: register_value
        type: u2
  multiple_coils:
    seq:
      - id: starting_address
        type: u2
      - id: num_values
        type: u2
      - id: byte_count
        type: u1
      - id: values
        type: b1
        repeat: expr
        repeat-expr: num_values
  multiple_registers:
    seq:
      - id: starting_address
        type: u2
      - id: num_register_values
        type: u2
      - id: byte_count
        type: u1
      - id: register_values
        type: u2
        repeat: expr
        repeat-expr: num_register_values