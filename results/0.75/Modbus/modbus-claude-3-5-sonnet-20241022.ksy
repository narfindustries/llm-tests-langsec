meta:
  id: modbus
  file-extension: mb
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
        0x01: read_coils_request
        0x02: read_discrete_inputs_request
        0x03: read_holding_registers_request
        0x04: read_input_registers_request
        0x05: write_single_coil_request
        0x06: write_single_register_request
        0x0f: write_multiple_coils_request
        0x10: write_multiple_registers_request
        0x81: exception_response
        0x82: exception_response
        0x83: exception_response
        0x84: exception_response
        0x85: exception_response
        0x86: exception_response
        0x8f: exception_response
        0x90: exception_response

types:
  read_coils_request:
    seq:
      - id: starting_address
        type: u2
      - id: quantity
        type: u2

  read_discrete_inputs_request:
    seq:
      - id: starting_address
        type: u2
      - id: quantity
        type: u2

  read_holding_registers_request:
    seq:
      - id: starting_address
        type: u2
      - id: quantity
        type: u2

  read_input_registers_request:
    seq:
      - id: starting_address
        type: u2
      - id: quantity
        type: u2

  write_single_coil_request:
    seq:
      - id: output_address
        type: u2
      - id: output_value
        type: u2

  write_single_register_request:
    seq:
      - id: register_address
        type: u2
      - id: register_value
        type: u2

  write_multiple_coils_request:
    seq:
      - id: starting_address
        type: u2
      - id: quantity
        type: u2
      - id: byte_count
        type: u1
      - id: coil_values
        size: byte_count

  write_multiple_registers_request:
    seq:
      - id: starting_address
        type: u2
      - id: quantity
        type: u2
      - id: byte_count
        type: u1
      - id: register_values
        size: byte_count

  exception_response:
    seq:
      - id: exception_code
        type: u1
        enum: exception_codes

enums:
  exception_codes:
    1: illegal_function
    2: illegal_data_address
    3: illegal_data_value
    4: server_device_failure
    5: acknowledge
    6: server_device_busy
    8: memory_parity_error
    10: gateway_path_unavailable
    11: gateway_target_device_failed_to_respond