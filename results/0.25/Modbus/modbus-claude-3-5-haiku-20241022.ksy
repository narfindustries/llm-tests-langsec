meta:
  id: modbus
  endian: be
  title: Modbus Protocol
  license: MIT

seq:
  - id: mbap_header
    type: mbap_header
    doc: Modbus Application Protocol Header
  - id: pdu
    type: protocol_data_unit

types:
  mbap_header:
    seq:
      - id: transaction_id
        type: u2
        doc: Identifies the transaction
      - id: protocol_id
        type: u2
        doc: Always 0 for Modbus
      - id: length
        type: u2
        doc: Number of following bytes
      - id: unit_id
        type: u1
        doc: Slave/Unit address

  protocol_data_unit:
    seq:
      - id: function_code
        type: u1
        enum: function_codes
      - id: data
        type:
          switch-on: function_code
          cases:
            'function_codes::read_coils': read_coils_request
            'function_codes::read_discrete_inputs': read_discrete_inputs_request
            'function_codes::read_holding_registers': read_holding_registers_request
            'function_codes::read_input_registers': read_input_registers_request
            'function_codes::write_single_coil': write_single_coil_request
            'function_codes::write_single_register': write_single_register_request
            'function_codes::write_multiple_coils': write_multiple_coils_request
            'function_codes::write_multiple_registers': write_multiple_registers_request
            'function_codes::read_file_record': read_file_record_request
            'function_codes::write_file_record': write_file_record_request
            'function_codes::mask_write_register': mask_write_register_request
            'function_codes::read_write_multiple_registers': read_write_multiple_registers_request
            'function_codes::read_device_identification': read_device_identification_request
            _: raw_request

  read_coils_request:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_coils
        type: u2
        valid:
          min: 1
          max: 2000

  read_discrete_inputs_request:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_inputs
        type: u2
        valid:
          min: 1
          max: 2000

  read_holding_registers_request:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_registers
        type: u2
        valid:
          min: 1
          max: 125

  read_input_registers_request:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_registers
        type: u2
        valid:
          min: 1
          max: 125

  write_single_coil_request:
    seq:
      - id: output_address
        type: u2
      - id: output_value
        type: u2
        enum: coil_states

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
      - id: quantity_of_coils
        type: u2
        valid:
          min: 1
          max: 1968
      - id: byte_count
        type: u1
      - id: coil_values
        type: u1
        repeat: expr
        repeat-expr: byte_count

  write_multiple_registers_request:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_registers
        type: u2
        valid:
          min: 1
          max: 123
      - id: byte_count
        type: u1
      - id: register_values
        type: u2
        repeat: expr
        repeat-expr: quantity_of_registers

  read_file_record_request:
    seq:
      - id: byte_count
        type: u1
      - id: records
        type: file_record
        repeat: expr
        repeat-expr: (byte_count - 1) / 7

  file_record:
    seq:
      - id: reference_type
        type: u1
        enum: reference_types
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
      - id: records
        type: write_file_record
        repeat: expr
        repeat-expr: (byte_count - 1) / 7

  write_file_record:
    seq:
      - id: reference_type
        type: u1
        enum: reference_types
      - id: file_number
        type: u2
      - id: record_number
        type: u2
      - id: record_length
        type: u2
      - id: record_data
        type: u2
        repeat: expr
        repeat-expr: record_length

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
      - id: quantity_to_read
        type: u2
        valid:
          min: 1
          max: 125
      - id: write_starting_address
        type: u2
      - id: quantity_to_write
        type: u2
        valid:
          min: 1
          max: 123
      - id: write_byte_count
        type: u1
      - id: write_register_values
        type: u2
        repeat: expr
        repeat-expr: quantity_to_write

  read_device_identification_request:
    seq:
      - id: mei_type
        type: u1
        enum: mei_types
      - id: object_id
        type: u1
      - id: object_value
        type: u1

  raw_request:
    seq:
      - id: raw_data
        type: u1
        repeat: eos

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
    0x14: read_file_record
    0x15: write_file_record
    0x16: mask_write_register
    0x17: read_write_multiple_registers
    0x2B: read_device_identification

  coil_states:
    0x0000: off
    0xFF00: on

  reference_types:
    0x06: request_record_access

  mei_types:
    0x0E: read_device_identification