meta:
  id: modbus
  file-extension: modbus
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
    enum: function_codes
  - id: data
    type: 
      switch-on: function_code
      cases:
        'function_codes::read_coils': read_request
        'function_codes::read_discrete_inputs': read_request
        'function_codes::read_holding_registers': read_request
        'function_codes::read_input_registers': read_request
        'function_codes::write_single_coil': write_single_request
        'function_codes::write_single_register': write_single_request
        'function_codes::write_multiple_coils': write_multiple_coils_request
        'function_codes::write_multiple_registers': write_multiple_registers_request
        _: raw_data

types:
  read_request:
    seq:
      - id: starting_address
        type: u2
      - id: quantity
        type: u2

  write_single_request:
    seq:
      - id: output_address
        type: u2
      - id: output_value
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

  raw_data:
    seq:
      - id: bytes
        size-eos: true

enums:
  function_codes:
    1: read_coils
    2: read_discrete_inputs
    3: read_holding_registers
    4: read_input_registers
    5: write_single_coil
    6: write_single_register
    15: write_multiple_coils
    16: write_multiple_registers

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