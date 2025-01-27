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
        0x01: read_coils
        0x02: read_discrete_inputs
        0x03: read_holding_registers
        0x04: read_input_registers
        0x05: write_single_coil
        0x06: write_single_register
        0x0f: write_multiple_coils
        0x10: write_multiple_registers
      types:
        read_coils:
          seq:
            - id: address
              type: u2
            - id: quantity
              type: u2
        read_discrete_inputs:
          seq:
            - id: address
              type: u2
            - id: quantity
              type: u2
        read_holding_registers:
          seq:
            - id: address
              type: u2
            - id: quantity
              type: u2
        read_input_registers:
          seq:
            - id: address
              type: u2
            - id: quantity
              type: u2
        write_single_coil:
          seq:
            - id: address
              type: u2
            - id: value
              type: u2
        write_single_register:
          seq:
            - id: address
              type: u2
            - id: value
              type: u2
        write_multiple_coils:
          seq:
            - id: address
              type: u2
            - id: quantity
              type: u2
            - id: byte_count
              type: u1
            - id: values
              type: u1
              repeat: byte_count
        write_multiple_registers:
          seq:
            - id: address
              type: u2
            - id: quantity
              type: u2
            - id: byte_count
              type: u1
            - id: values
              type: u2
              repeat: byte_count / 2