types:
  modbus_data:
    switch-on: function_code
    cases:
      - 1:
          seq:
            - id: starting_address
              type: u2
            - id: quantity
              type: u2
            - id: coil_status
              type: bits
              size: quantity
      - 3:
          seq:
            - id: starting_address
              type: u2
            - id: quantity
              type: u2
            - id: holding_registers
              type: u2
              repeat: expr
              repeat-expr: quantity
      - 4:
          seq:
            - id: starting_address
              type: u2
            - id: quantity
              type: u2
            - id: input_registers
              type: u2
              repeat: expr
              repeat-expr: quantity
      - 5:
          seq:
            - id: starting_address
              type: u2
            - id: quantity
              type: u2
            - id: discrete_inputs
              type: bits
              size: quantity
      - 6:
          seq:
            - id: starting_address
              type: u2
            - id: value
              type: u2
      - 15:
          seq:
            - id: starting_address
              type: u2
            - id: quantity
              type: u2
            - id: byte_count
              type: u1
            - id: data_bytes
              type: u1
              repeat: expr
              repeat-expr: byte_count
      - 16:
          seq:
            - id: starting_address
              type: u2
            - id: quantity
              type: u2
            - id: byte_count
              type: u1
            - id: data_bytes
              type: u1
              repeat: expr
              repeat-expr: byte_count
      - default:
          seq:
            - id: data_bytes
              type: u1
              repeat: expr
              repeat-expr: length

modbus:
  endian: be
  seq:
    - id: transaction_identifier
      type: u2
    - id: protocol_identifier
      type: u2
    - id: length
      type: u2
    - id: unit_identifier
      type: u1
    - id: function_code
      type: u1
    - id: data
      type: modbus_data
      size: length

