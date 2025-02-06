meta:
  id: modbus
  endian: be
types:
  byte_array:
    type: seq
    seq:
      - id: len
        type: u1
      - id: data
        type: u1
        repeat: expr
        repeat-expr: len
seq:
  - id: address
    type: u1
  - id: function_code
    type: u1
  - id: data
    type:
      switch-on: function_code
      cases:
        0x01: read_coil_status
        0x02: read_input_status
        0x03: read_holding_registers
        0x04: read_input_registers
        0x05: force_single_coil
        0x06: preset_single_register
        0x07: read_write_multiple_coils
        0x0f: write_multiple_coils
        0x10: write_multiple_registers
        0x11: report_slave_id
        0x14: read_file_record
        0x15: write_file_record
        0x16: mask_write_register
        0x17: read_write_multiple_registers
        0x18: read_fifo_queue
        0x2b: encapsulated_interface_transport
types:
  read_coil_status:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_coils
        type: u2
      - id: byte_count
        type: u1
      - id: coil_status
        type: byte_array
        size: byte_count
      - id: crc
        type: u2
  read_input_status:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_inputs
        type: u2
      - id: byte_count
        type: u1
      - id: input_status
        type: byte_array
        size: byte_count
      - id: crc
        type: u2
  read_holding_registers:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_registers
        type: u2
      - id: byte_count
        type: u1
      - id: register_values
        type: u2
        repeat: expr
        repeat-expr: quantity_of_registers
      - id: crc
        type: u2
  read_input_registers:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_registers
        type: u2
      - id: byte_count
        type: u1
      - id: register_values
        type: u2
        repeat: expr
        repeat-expr: quantity_of_registers
      - id: crc
        type: u2
  force_single_coil:
    seq:
      - id: output_address
        type: u2
      - id: output_value
        type: u2
      - id: crc
        type: u2
  preset_single_register:
    seq:
      - id: register_address
        type: u2
      - id: register_value
        type: u2
      - id: crc
        type: u2
  read_write_multiple_coils:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_coils
        type: u2
      - id: byte_count
        type: u1
      - id: coil_status
        type: byte_array
        size: byte_count
      - id: crc
        type: u2
  write_multiple_coils:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_coils
        type: u2
      - id: byte_count
        type: u1
      - id: coil_values
        type: byte_array
        size: byte_count
      - id: crc
        type: u2
  write_multiple_registers:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_registers
        type: u2
      - id: byte_count
        type: u1
      - id: register_values
        type: u2
        repeat: expr
        repeat-expr: quantity_of_registers
      - id: crc
        type: u2
  report_slave_id:
    seq:
      - id: slave_id
        type: u1
      - id: padding
        type: u1
      - id: runtime_exception_status
        type: u1
      - id: length
        type: u1
      - id: data
        type: byte_array
        size: length
      - id: crc
        type: u2
  read_file_record:
    seq:
      - id: reference_type
        type: u1
      - id: file_number
        type: u2
      - id: record_number
        type: u2
      - id: record_length
        type: u2
      - id: data
        type: byte_array
        size: record_length
      - id: crc
        type: u2
  write_file_record:
    seq:
      - id: reference_type
        type: u1
      - id: file_number
        type: u2
      - id: record_number
        type: u2
      - id: record_length
        type: u2
      - id: data
        type: byte_array
        size: record_length
      - id: crc
        type: u2
  mask_write_register:
    seq:
      - id: reference_type
        type: u1
      - id: and_mask
        type: u2
      - id: or_mask
        type: u2
      - id: crc
        type: u2
  read_write_multiple_registers:
    seq:
      - id: read_starting_address
        type: u2
      - id: quantity_of_registers_to_read
        type: u2
      - id: write_starting_address
        type: u2
      - id: quantity_of_registers_to_write
        type: u2
      - id: byte_count
        type: u1
      - id: register_values
        type: u2
        repeat: expr
        repeat-expr: quantity_of_registers_to_write
      - id: crc
        type: u2
  read_fifo_queue:
    seq:
      - id: fifo_pointer_address
        type: u2
      - id: quantity_of_registers_to_read
        type: u2
      - id: byte_count
        type: u1
      - id: register_values
        type: u2
        repeat: expr
        repeat-expr: quantity_of_registers_to_read
      - id: crc
        type: u2
  encapsulated_interface_transport:
    seq:
      - id: mei_type
        type: u2
      - id: mei_data_len
        type: u1
      - id: mei_data
        type: u1
        repeat: expr
        repeat-expr: mei_data_len
      - id: crc
        type: u2