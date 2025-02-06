meta:
  id: modbus
  file-extension: mdb
  endian: be

seq:
  - id: mbap_header
    type: mbap_header
    if: _io.size >= 7
  - id: pdu
    type: pdu

types:
  mbap_header:
    seq:
      - id: transaction_id
        type: u2
      - id: protocol_id
        type: u2
      - id: length
        type: u2
      - id: unit_id
        type: u1

  pdu:
    seq:
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
            0x81: error_response
            0x82: error_response
            0x83: error_response
            0x84: error_response
            0x85: error_response
            0x86: error_response
            0x8f: error_response
            0x90: error_response

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

  error_response:
    seq:
      - id: exception_code
        type: u1
        enum: exception_codes

enums:
  exception_codes:
    0x01: illegal_function
    0x02: illegal_data_address
    0x03: illegal_data_value
    0x04: server_device_failure
    0x05: acknowledge
    0x06: server_device_busy
    0x08: memory_parity_error
    0x0a: gateway_path_unavailable
    0x0b: gateway_target_device_failed

  function_codes:
    0x01: read_coils
    0x02: read_discrete_inputs
    0x03: read_holding_registers
    0x04: read_input_registers
    0x05: write_single_coil
    0x06: write_single_register
    0x0f: write_multiple_coils
    0x10: write_multiple_registers