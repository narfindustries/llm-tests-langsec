meta:
  id: modbus
  file-extension: mdb
  endian: be

seq:
  - id: mbap_header
    type: mbap_header
    if: _io.size >= 7  # MBAP header is only present in TCP/IP
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
            0x14: read_file_record_request
            0x15: write_file_record_request
            0x16: mask_write_register_request
            0x17: read_write_multiple_registers_request
            0x18: read_fifo_queue_request
            0x81: error_response
            0x82: error_response
            0x83: error_response
            0x84: error_response
            0x85: error_response
            0x86: error_response
            0x8f: error_response
            0x90: error_response
            0x94: error_response
            0x95: error_response
            0x96: error_response
            0x97: error_response
            0x98: error_response

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

  read_file_record_request:
    seq:
      - id: byte_count
        type: u1
      - id: subrequests
        type: file_record_request
        repeat: expr
        repeat-expr: byte_count / 7

  file_record_request:
    seq:
      - id: reference_type
        type: u1
      - id: file_number
        type: u2
      - id: record_number
        type: u2
      - id: record_length
        type: u2

  write_file_record_request:
    seq:
      - id: byte_count
        type: u1
      - id: subrequests
        type: file_record_write_request
        repeat: eos

  file_record_write_request:
    seq:
      - id: reference_type
        type: u1
      - id: file_number
        type: u2
      - id: record_number
        type: u2
      - id: record_length
        type: u2
      - id: record_data
        size: record_length * 2

  mask_write_register_request:
    seq:
      - id: reference_address
        type: u2
      - id: and_mask
        type: u2
      - id: or_mask
        type: u2

  read_write_multiple_registers_request:
    seq:
      - id: read_starting_address
        type: u2
      - id: read_quantity
        type: u2
      - id: write_starting_address
        type: u2
      - id: write_quantity
        type: u2
      - id: write_byte_count
        type: u1
      - id: write_registers
        size: write_byte_count

  read_fifo_queue_request:
    seq:
      - id: fifo_pointer_address
        type: u2

  error_response:
    seq:
      - id: exception_code
        type: u1
        enum: exception_codes

enums:
  exception_codes:
    1: illegal_function
    2: illegal_data_address
    3: illegal_data_value
    4: slave_device_failure
    5: acknowledge
    6: slave_device_busy
    8: memory_parity_error
    10: gateway_path_unavailable
    11: gateway_target_device_failed_to_respond