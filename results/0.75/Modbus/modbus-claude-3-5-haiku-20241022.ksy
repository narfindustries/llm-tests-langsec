meta:
  id: modbus
  endian: be
seq:
  - id: mbap_header
    type: mbap_header
  - id: pdu
    type: pdu
types:
  mbap_header:
    seq:
      - id: transaction_id
        type: u2
      - id: protocol_id
        type: u2
        valid: 0x0000
      - id: length
        type: u2
      - id: unit_id
        type: u1
  pdu:
    seq:
      - id: function_code
        type: u1
        enum: function_codes
    types:
      read_coils:
        seq:
          - id: start_address
            type: u2
          - id: quantity
            type: u2
            valid:
              max: 2000
      read_discrete_inputs:
        seq:
          - id: start_address
            type: u2
          - id: quantity
            type: u2
            valid:
              max: 2000
      read_holding_registers:
        seq:
          - id: start_address
            type: u2
          - id: quantity
            type: u2
            valid:
              max: 125
      read_input_registers:
        seq:
          - id: start_address
            type: u2
          - id: quantity
            type: u2
            valid:
              max: 125
      write_single_coil:
        seq:
          - id: address
            type: u2
          - id: value
            type: u2
            enum: coil_values
      write_single_register:
        seq:
          - id: address
            type: u2
          - id: value
            type: u2
      write_multiple_coils:
        seq:
          - id: start_address
            type: u2
          - id: quantity
            type: u2
            valid:
              max: 1968
          - id: byte_count
            type: u1
          - id: coil_values
            type: u1
            repeat: expr
            repeat-expr: byte_count
      write_multiple_registers:
        seq:
          - id: start_address
            type: u2
          - id: quantity
            type: u2
            valid:
              max: 123
          - id: byte_count
            type: u1
          - id: register_values
            type: u2
            repeat: expr
            repeat-expr: quantity
    instances:
      request_data:
        io: _root._io
        pos: _io.pos
        type:
          switch-on: function_code
          cases:
            'function_codes::read_coils': read_coils
            'function_codes::read_discrete_inputs': read_discrete_inputs
            'function_codes::read_holding_registers': read_holding_registers
            'function_codes::read_input_registers': read_input_registers
            'function_codes::write_single_coil': write_single_coil
            'function_codes::write_single_register': write_single_register
            'function_codes::write_multiple_coils': write_multiple_coils
            'function_codes::write_multiple_registers': write_multiple_registers
  exception_response:
    seq:
      - id: function_code
        type: u1
      - id: exception_code
        type: u1
        enum: exception_codes
enums:
  function_codes:
    0x01: read_coils
    0x02: read_discrete_inputs
    0x03: read_holding_registers
    0x04: read_input_registers
    0x05: write_single_coil
    0x06: write_single_register
    0x0F: write_multiple_coils
    0x10: write_multiple_registers
  coil_values:
    0x0000: off
    0xFF00: on
  exception_codes:
    0x01: illegal_function
    0x02: illegal_data_address
    0x03: illegal_data_value
    0x04: server_device_failure
    0x05: acknowledge
    0x06: server_device_busy
    0x07: negative_acknowledge
    0x08: memory_parity_error
    0x0A: gateway_path_unavailable
    0x0B: gateway_target_device_failed_to_respond