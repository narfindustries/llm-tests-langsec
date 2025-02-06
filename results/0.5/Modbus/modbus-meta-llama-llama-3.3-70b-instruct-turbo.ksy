meta:
  id: modbus
  title: Modbus Protocol
  endian: big
seq:
  - id: slave_id
    type: u1
  - id: function_code
    type: u1
  - id: data
    type:
      switch-on: function_code
      cases:
        1: read_coil_status
        2: read_input_status
        3: read_holding_registers
        4: read_input_registers
        5: write_single_coil
        6: write_single_register
        15: write_multiple_coils
        16: write_multiple_registers
        17: report_slave_id
        20: read_file_record
        21: write_file_record
        22: mask_write_register
        23: read_write_multiple_registers
        24: read_fifo_queue
types:
  read_coil_status:
    seq:
      - id: byte_count
        type: u1
      - id: coil_status
        type: u1
        repeat: byte_count
  read_input_status:
    seq:
      - id: byte_count
        type: u1
      - id: input_status
        type: u1
        repeat: byte_count
  read_holding_registers:
    seq:
      - id: byte_count
        type: u1
      - id: register_values
        type: u2
        repeat: expr: byte_count / 2
  read_input_registers:
    seq:
      - id: byte_count
        type: u1
      - id: register_values
        type: u2
        repeat: expr: byte_count / 2
  write_single_coil:
    seq:
      - id: coil_address
        type: u2
      - id: coil_value
        type: u2
  write_single_register:
    seq:
      - id: register_address
        type: u2
      - id: register_value
        type: u2
  write_multiple_coils:
    seq:
      - id: starting_coil_address
        type: u2
      - id: number_of_coils
        type: u2
      - id: coil_values
        type: u1
        repeat: expr: number_of_coils / 8
  write_multiple_registers:
    seq:
      - id: starting_register_address
        type: u2
      - id: number_of_registers
        type: u2
      - id: register_values
        type: u2
        repeat: expr: number_of_registers
  report_slave_id:
    seq:
      - id: slave_id
        type: u2
      - id: run_indicator
        type: u1
  read_file_record:
    seq:
      - id: file_number
        type: u1
      - id: record_number
        type: u1
      - id: record_length
        type: u1
  write_file_record:
    seq:
      - id: file_number
        type: u1
      - id: record_number
        type: u1
      - id: record_length
        type: u1
      - id: record_data
        type: u1
        repeat: expr: record_length
  mask_write_register:
    seq:
      - id: register_address
        type: u2
      - id: and_mask
        type: u2
      - id: or_mask
        type: u2
  read_write_multiple_registers:
    seq:
      - id: read_starting_register_address
        type: u2
      - id: number_of_registers_to_read
        type: u2
      - id: write_starting_register_address
        type: u2
      - id: number_of_registers_to_write
        type: u2
      - id: register_values
        type: u2
        repeat: expr: number_of_registers_to_write
  read_fifo_queue:
    seq:
      - id: queue_address
        type: u2
  exception_response:
    seq:
      - id: function_code
        type: u1
      - id: error_code
        type: u1
      - id: data
        type:
          switch-on: function_code
          cases:
            1: read_coil_status
            2: read_input_status
            3: read_holding_registers
            4: read_input_registers
            5: write_single_coil
            6: write_single_register
            15: write_multiple_coils
            16: write_multiple_registers
            17: report_slave_id
            20: read_file_record
            21: write_file_record
            22: mask_write_register
            23: read_write_multiple_registers
            24: read_fifo_queue
  crc:
    seq:
      - id: crc_lo
        type: u1
      - id: crc_hi
        type: u1