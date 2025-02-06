meta:
  id: modbus
  title: Modbus Protocol
  file-extension: modbus
  license: MIT
  endian: be
seq:
  - id: transaction_id
    type: u2
    if: _root.is_tcp
  - id: protocol_id
    type: u2
    if: _root.is_tcp
  - id: length
    type: u2
    if: _root.is_tcp
  - id: unit_id
    type: u1
    if: _root.is_tcp
  - id: slave_address
    type: u1
    if: _root.is_rtu or _root.is_ascii
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
        0x07: read_exception_status
        0x08: diagnostics
        0x0B: get_comm_event_counter
        0x0C: get_comm_event_log
        0x0F: write_multiple_coils
        0x10: write_multiple_registers
        0x11: report_server_id
        0x14: read_file_record
        0x15: write_file_record
        0x16: mask_write_register
        0x17: read_write_multiple_registers
        0x18: read_fifo_queue
        0x2B: encapsulated_interface_transport
  - id: crc
    type: u2
    if: _root.is_rtu
  - id: start_char
    type: str
    encoding: ASCII
    size: 1
    if: _root.is_ascii
  - id: lrc
    type: u1
    if: _root.is_ascii
  - id: end_chars
    type: str
    encoding: ASCII
    size: 2
    if: _root.is_ascii
types:
  read_coils:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_coils
        type: u2
  read_discrete_inputs:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_inputs
        type: u2
  read_holding_registers:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_registers
        type: u2
  read_input_registers:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_registers
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
    seq: []
  diagnostics:
    seq:
      - id: sub_function
        type: u2
      - id: data
        type: u2
  get_comm_event_counter:
    seq: []
  get_comm_event_log:
    seq: []
  write_multiple_coils:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_outputs
        type: u2
      - id: byte_count
        type: u1
      - id: outputs_value
        type: u1
        repeat: expr
        repeat-expr: byte_count
  write_multiple_registers:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_registers
        type: u2
      - id: byte_count
        type: u1
      - id: registers_value
        type: u1
        repeat: expr
        repeat-expr: byte_count
  report_server_id:
    seq: []
  read_file_record:
    seq:
      - id: byte_count
        type: u1
      - id: reference_type
        type: u1
      - id: file_number
        type: u2
      - id: record_number
        type: u2
      - id: record_length
        type: u2
  write_file_record:
    seq:
      - id: byte_count
        type: u1
      - id: reference_type
        type: u1
      - id: file_number
        type: u2
      - id: record_number
        type: u2
      - id: record_data
        type: u1
        repeat: expr
        repeat-expr: record_length
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
      - id: read_starting_address
        type: u2
      - id: quantity_to_read
        type: u2
      - id: write_starting_address
        type: u2
      - id: quantity_to_write
        type: u2
      - id: write_byte_count
        type: u1
      - id: write_registers_value
        type: u1
        repeat: expr
        repeat-expr: write_byte_count
  read_fifo_queue:
    seq:
      - id: fifo_pointer_address
        type: u2
  encapsulated_interface_transport:
    seq:
      - id: mei_type
        type: u1
      - id: mei_data
        type: u1
        repeat: eos
enums:
  function_code:
    0x01: read_coils
    0x02: read_discrete_inputs
    0x03: read_holding_registers
    0x04: read_input_registers
    0x05: write_single_coil
    0x06: write_single_register
    0x07: read_exception_status
    0x08: diagnostics
    0x0B: get_comm_event_counter
    0x0C: get_comm_event_log
    0x0F: write_multiple_coils
    0x10: write_multiple_registers
    0x11: report_server_id
    0x14: read_file_record
    0x15: write_file_record
    0x16: mask_write_register
    0x17: read_write_multiple_registers
    0x18: read_fifo_queue
    0x2B: encapsulated_interface_transport
instances:
  is_tcp:
    value: true
  is_rtu:
    value: not _root.is_tcp and not _root.is_ascii
  is_ascii:
    value: false