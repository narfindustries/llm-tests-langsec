format modbus_message {
  address_field: uint8;
  function_code: uint8;
  data_field: choice {
    case 0x01: read_coil_status;
    case 0x02: read_input_status;
    case 0x03: read_holding_registers;
    case 0x04: read_input_registers;
    case 0x05: write_single_coil;
    case 0x06: write_single_holding_register;
    case 0x0F: write_multiple_coils;
    case 0x10: write_multiple_holding_registers;
    case 0x11: report_slave_id;
    case 0x14: read_file_record;
    case 0x15: write_file_record;
    case 0x16: mask_write_register;
    case 0x17: read_write_multiple_registers;
    case 0x18: read_fifo_queue;
    case 0x2B: encapsulated_interface_transport;
  }
  crc_16: uint16;
}

format read_coil_status {
  number_of_coils: uint8;
  coil_status: bytes (number_of_coils / 8 + (number_of_coils % 8 != 0));
}

format read_input_status {
  number_of_inputs: uint8;
  input_status: bytes (number_of_inputs / 8 + (number_of_inputs % 8 != 0));
}

format read_holding_registers {
  starting_address: uint16;
  number_of_registers: uint16;
  byte_count: uint8;
  register_values: bytes (byte_count);
}

format read_input_registers {
  starting_address: uint16;
  number_of_registers: uint16;
  byte_count: uint8;
  register_values: bytes (byte_count);
}

format write_single_coil {
  output_address: uint16;
  output_value: uint16;
}

format write_single_holding_register {
  register_address: uint16;
  register_value: uint16;
}

format write_multiple_coils {
  starting_address: uint16;
  number_of_coils: uint16;
  byte_count: uint8;
  coil_values: bytes (byte_count);
}

format write_multiple_holding_registers {
  starting_address: uint16;
  number_of_registers: uint16;
  byte_count: uint8;
  register_values: bytes (byte_count);
}

format report_slave_id {
  slave_id: uint8;
  run_indicator_status: uint8;
  data: bytes (*);
}

format read_file_record {
  reference_number: uint16;
  file_number: uint16;
  record_number: uint16;
  record_length: uint16;
  data: bytes (record_length);
}

format write_file_record {
  reference_number: uint16;
  file_number: uint16;
  record_number: uint16;
  record_length: uint16;
  data: bytes (record_length);
}

format mask_write_register {
  reference_number: uint16;
  and_mask: uint16;
  or_mask: uint16;
}

format read_write_multiple_registers {
  read_starting_address: uint16;
  number_of_registers_to_read: uint16;
  write_starting_address: uint16;
  number_of_registers_to_write: uint16;
  byte_count: uint8;
  register_values: bytes (byte_count);
}

format read_fifo_queue {
  fifo_pointer_address: uint16;
  number_of_fifo_values: uint16;
  data: bytes (number_of_fifo_values * 2);
}

format encapsulated_interface_transport {
  mei_type: uint8;
  mei_data: bytes (*);
}