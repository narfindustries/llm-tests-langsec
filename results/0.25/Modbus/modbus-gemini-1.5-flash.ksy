types:
  modbus_pdu:
    seq:
      - id: function_code
        type: u1
      - id: data
        type: modbus_data
        instance: function_code
  modbus_data:
    switch: function_code
    cases:
      0x01:
        seq:
          - id: starting_address
            type: u2
          - id: quantity
            type: u2
          - id: byte_count
            type: u1
          - id: coils
            type: bits
            repeat: expr
            expr: byte_count
      0x02:
        seq:
          - id: starting_address
            type: u2
          - id: quantity
            type: u2
          - id: byte_count
            type: u1
          - id: inputs
            type: bits
            repeat: expr
            expr: byte_count
      0x03:
        seq:
          - id: starting_address
            type: u2
          - id: quantity
            type: u2
          - id: byte_count
            type: u1
          - id: holding_registers
            type: u2
            repeat: expr
            expr: byte_count / 2
      0x04:
        seq:
          - id: starting_address
            type: u2
          - id: quantity
            type: u2
          - id: byte_count
            type: u1
          - id: input_registers
            type: u2
            repeat: expr
            expr: byte_count / 2
      0x05:
        seq:
          - id: starting_address
            type: u2
          - id: value
            type: u2
      0x06:
        seq:
          - id: starting_address
            type: u2
          - id: value
            type: u2
      0x0f:
        seq:
          - id: starting_address
            type: u2
          - id: quantity
            type: u2
          - id: byte_count
            type: u1
          - id: coils
            type: u2
            repeat: expr
            expr: byte_count / 2
      0x10:
        seq:
          - id: starting_address
            type: u2
          - id: quantity
            type: u2
          - id: byte_count
            type: u1
          - id: registers
            type: u2
            repeat: expr
            expr: byte_count / 2
      0x17:
        seq:
          - id: starting_address
            type: u2
          - id: quantity
            type: u2
          - id: data
            type: u1
            repeat: expr
            expr: quantity
      0x18:
        seq:
          - id: starting_address
            type: u2
          - id: quantity
            type: u2
          - id: data
            type: u2
            repeat: expr
            expr: quantity
      0x00:
        seq:
          - id: exception_code
            type: u1
  modbus_rtu:
    seq:
      - id: pdu
        type: modbus_pdu
  modbus_ascii:
    seq:
      - id: pdu
        type: modbus_pdu
  modbus_tcp:
    seq:
      - id: header
        type: u2
      - id: pdu
        type: modbus_pdu

