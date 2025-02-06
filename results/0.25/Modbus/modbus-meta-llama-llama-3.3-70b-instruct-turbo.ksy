meta:
  id: modbus
  title: Modbus Protocol
  endian: be
seq:
  - id: slave_id
    type: u1
    doc: The address of the slave device that the message is intended for.
  - id: function_code
    type: u1
    doc: The function code indicates the type of action to be performed by the slave device.
  - id: data
    type:
      switch-on: function_code
      cases:
        0x01: read_coil_status_data
        0x02: read_input_status_data
        0x03: read_holding_registers_data
        0x04: read_input_registers_data
        0x05: write_single_coil_data
        0x06: write_single_register_data
        0x07: read_exception_status_data
        0x08: diagnostics_data
        0x0b: get_com_event_counter_data
        0x0c: get_com_event_log_data
        0x0f: write_multiple_coils_data
        0x10: write_multiple_registers_data
        0x11: report_slave_id_data
        0x12: program_884_data
        0x14: read_file_record_data
        0x15: write_file_record_data
        0x16: mask_write_register_data
        0x17: read_write_multiple_registers_data
        0x18: read_fifo_queue_data
types:
  read_coil_status_data:
    seq:
      - id: starting_address
        type: u2
      - id: quantity
        type: u2
  read_input_status_data:
    seq:
      - id: starting_address
        type: u2
      - id: quantity
        type: u2
  read_holding_registers_data:
    seq:
      - id: starting_address
        type: u2
      - id: quantity
        type: u2
  read_input_registers_data:
    seq:
      - id: starting_address
        type: u2
      - id: quantity
        type: u2
  write_single_coil_data:
    seq:
      - id: starting_address
        type: u2
      - id: value
        type: u2
  write_single_register_data:
    seq:
      - id: starting_address
        type: u2
      - id: value
        type: u2
  read_exception_status_data:
    seq:
      - id: status
        type: u1
  diagnostics_data:
    seq:
      - id: sub_function
        type: u1
      - id: data
        type: u2
  get_com_event_counter_data:
    seq:
      - id: status
        type: u2
  get_com_event_log_data:
    seq:
      - id: status
        type: u2
      - id: message
        type: str
        size: 64
  write_multiple_coils_data:
    seq:
      - id: starting_address
        type: u2
      - id: quantity
        type: u2
      - id: byte_count
        type: u1
      - id: values
        type:
          repeat: expr
          expr: byte_count
          type: u1
  write_multiple_registers_data:
    seq:
      - id: starting_address
        type: u2
      - id: quantity
        type: u2
      - id: byte_count
        type: u1
      - id: values
        type:
          repeat: expr
          expr: byte_count / 2
          type: u2
  report_slave_id_data:
    seq:
      - id: slave_id
        type: u1
      - id: run_indicator_status
        type: u1
  program_884_data:
    seq:
      - id: status
        type: u1
  read_file_record_data:
    seq:
      - id: reference_type
        type: u1
      - id: file_number
        type: u2
      - id: record_number
        type: u2
      - id: record_length
        type: u2
  write_file_record_data:
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
        type:
          repeat: expr
          expr: record_length
          type: u1
  mask_write_register_data:
    seq:
      - id: reference_type
        type: u1
      - id: register_address
        type: u2
      - id: and_mask
        type: u2
      - id: or_mask
        type: u2
  read_write_multiple_registers_data:
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
      - id: write_values
        type:
          repeat: expr
          expr: write_byte_count / 2
          type: u2
  read_fifo_queue_data:
    seq:
      - id: starting_address
        type: u2
      - id: quantity
        type: u2
  crc16:
    seq:
      - id: crc
        type: u2
    doc: The cyclic redundancy check (CRC) of the message.
    if: _root._io.is_eof == false