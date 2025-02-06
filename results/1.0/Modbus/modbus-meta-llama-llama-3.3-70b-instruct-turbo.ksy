meta:
  id: modbus
  title: Modbus
types:
  read_coil_status:
    seq:
      - id: byte_count
        type: u1
      - id: coil_status
        type:
          repeat: expr
          repeat-expr: byte_count
          type: bits
  read_input_status:
    seq:
      - id: byte_count
        type: u1
      - id: input_status
        type:
          repeat: expr
          repeat-expr: byte_count
          type: bits
  read_holding_registers:
    seq:
      - id: byte_count
        type: u1
      - id: registers
        type:
          repeat: expr
          repeat-expr: byte_count / 2
          type: u2
  read_input_registers:
    seq:
      - id: byte_count
        type: u1
      - id: registers
        type:
          repeat: expr
          repeat-expr: byte_count / 2
          type: u2
  write_single_coil:
    seq:
      - id: output_address
        type: u2
      - id: output_value
        type: u2
  write_single_register:
    seq:
      - id: register_address
        type: u2
      - id: register_value
        type: u2
  read_exception_status:
    seq:
      - id: status
        type: u1
  write_multiple_coils:
    seq:
      - id: output_address
        type: u2
      - id: output_quantity
        type: u2
      - id: byte_count
        type: u1
      - id: output_values
        type:
          repeat: expr
          repeat-expr: byte_count
          type: bits
  write_multiple_registers:
    seq:
      - id: starting_address
        type: u2
      - id: quantity
        type: u2
      - id: byte_count
        type: u1
      - id: register_values
        type:
          repeat: expr
          repeat-expr: byte_count / 2
          type: u2
  report_slave_id:
    seq:
      - id: slave_id
        type: u1
      - id: run_indicator_status
        type: u1
      - id: additional_info
        type: str
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
      - id: record_data
        type: str
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
      - id: record_data
        type: str
  mask_write_register:
    seq:
      - id: reference_address
        type: u2
      - id: and_mask
        type: u2
      - id: or_mask
        type: u2
  read_write_multiple_registers:
    seq:
      - id: read_reference_address
        type: u2
      - id: read_quantity
        type: u2
      - id: write_reference_address
        type: u2
      - id: write_quantity
        type: u2
      - id: write_byte_count
        type: u1
      - id: write_register_values
        type:
          repeat: expr
          repeat-expr: write_byte_count / 2
          type: u2
  read_fifo_queue:
    seq:
      - id: fifo_pointer_address
        type: u2
      - id: fifo_quantity
        type: u2
  encapsulated_interface_transport:
    seq:
      - id: mei_type
        type: u2
      - id: mei_data
        type: str
  bits:
    type: b1
  u1:
    type: u1
  u2:
    type: u2
seq:
  - id: slave_id
    type: u1
  - id: function_code
    type: u1
  - id: body
    type:
      switch-on: function_code
      cases:
        0x01: read_coil_status
        0x02: read_input_status
        0x03: read_holding_registers
        0x04: read_input_registers
        0x05: write_single_coil
        0x06: write_single_register
        0x07: read_exception_status
        0x0f: write_multiple_coils
        0x10: write_multiple_registers
        0x11: report_slave_id
        0x14: read_file_record
        0x15: write_file_record
        0x16: mask_write_register
        0x17: read_write_multiple_registers
        0x18: read_fifo_queue
        0x2b: encapsulated_interface_transport