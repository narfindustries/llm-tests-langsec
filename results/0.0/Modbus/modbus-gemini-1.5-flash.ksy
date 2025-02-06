types:
  modbus_pdu:
    seq:
      - id: function_code
        type: u1
      - id: data
        type: modbus_data
        if: function_code < 0x80
      - id: exception_code
        type: u1
        if: function_code >= 0x80

  modbus_data:
    switch: function_code
    cases:
      0x01:
        seq:
          - id: byte_count
            type: u1
          - id: coil_status
            type: bits
            repeat: expr
            repeat-expr: byte_count * 8
      0x02:
        seq:
          - id: byte_count
            type: u1
          - id: input_status
            type: bits
            repeat: expr
            repeat-expr: byte_count * 8
      0x03:
        seq:
          - id: byte_count
            type: u1
          - id: holding_registers
            type: u2
            repeat: expr
            repeat-expr: byte_count / 2
      0x04:
        seq:
          - id: byte_count
            type: u1
          - id: input_registers
            type: u2
            repeat: expr
            repeat-expr: byte_count / 2
      0x05:
        seq:
          - id: starting_address
            type: u2
          - id: coil_value
            type: u1
      0x06:
        seq:
          - id: starting_address
            type: u2
          - id: register_value
            type: u2
      0x0f:
        seq:
          - id: starting_address
            type: u2
          - id: quantity
            type: u2
          - id: byte_count
            type: u1
          - id: coil_values
            type: u1
            repeat: expr
            repeat-expr: byte_count
      0x10:
        seq:
          - id: starting_address
            type: u2
          - id: quantity
            type: u2
          - id: byte_count
            type: u1
          - id: register_values
            type: u2
            repeat: expr
            repeat-expr: byte_count / 2
      0x11:
        seq:
          - id: starting_address
            type: u2
          - id: quantity
            type: u2
          - id: byte_count
            type: u1
          - id: coil_values
            type: u1
            repeat: expr
            repeat-expr: byte_count
      0x16:
        seq:
          - id: starting_address
            type: u2
          - id: register_value
            type: u2
      # Add other function codes as needed...  This is not exhaustive.

