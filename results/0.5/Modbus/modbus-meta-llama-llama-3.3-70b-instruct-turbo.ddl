def modbus(address: uint8, function_code: uint8, data: bytes, error_code: uint8, crc: uint16):
  address = range(0, 247)
  function_code = 
    0x01: read_coil_status
    | 0x02: read_input_status
    | 0x03: read_holding_registers
    | 0x04: read_input_registers
    | 0x05: write_single_coil
    | 0x06: write_single_holding_register
    | 0x0F: write_multiple_coils
    | 0x10: write_multiple_holding_registers
    | 0x11: report_slave_id
    | 0x14: read_file_record
    | 0x15: write_file_record
    | 0x16: mask_write_register
    | 0x17: read_write_multiple_registers
    | 0x18: read_fifo_queue
    | 0x2B: encapsulated_interface_transport

  data = switch function_code {
    case read_coil_status: 
      coil_status: bit[1..2000]
    case read_input_status: 
      input_status: bit[1..2000]
    case read_holding_registers: 
      register_values: uint16[1..125]
    case read_input_registers: 
      register_values: uint16[1..125]
    case write_single_coil: 
      coil_address: uint16
      coil_value: bit
    case write_single_holding_register: 
      register_address: uint16
      register_value: uint16
    case write_multiple_coils: 
      coil_addresses: uint16[1..196]
      coil_values: bit[1..196]
    case write_multiple_holding_registers: 
      register_addresses: uint16[1..125]
      register_values: uint16[1..125]
    case report_slave_id: 
      slave_id: uint8
      additional_info: byte[1..254]
    case read_file_record: 
      file_record_data: byte[1..252]
    case write_file_record: 
      file_record_data: byte[1..252]
    case mask_write_register: 
      register_address: uint16
      and_mask: uint16
      or_mask: uint16
    case read_write_multiple_registers: 
      register_addresses: uint16[1..125]
      register_values: uint16[1..125]
    case read_fifo_queue: 
      fifo_queue_data: byte[1..252]
    case encapsulated_interface_transport: 
      mei_type: uint8
      mei_data: byte[1..252]
  }

  error_code = 
    0x01: illegal_function
    | 0x02: illegal_data_address
    | 0x03: illegal_data_value
    | 0x04: slave_device_failure
    | 0x05: acknowledge
    | 0x06: slave_device_busy
    | 0x07: negative_acknowledge
    | 0x08: memory_parity_error
    | 0x0A: gateway_path_unavailable
    | 0x0B: gateway_target_device_failed_to_respond

  crc = calculate_crc(address, function_code, data)

def modbus_response(address: uint8, function_code: uint8, data: bytes, error_code: uint8, crc: uint16):
  address = range(0, 247)
  function_code = 
    0x01: read_coil_status
    | 0x02: read_input_status
    | 0x03: read_holding_registers
    | 0x04: read_input_registers
    | 0x05: write_single_coil
    | 0x06: write_single_holding_register
    | 0x0F: write_multiple_coils
    | 0x10: write_multiple_holding_registers
    | 0x11: report_slave_id
    | 0x14: read_file_record
    | 0x15: write_file_record
    | 0x16: mask_write_register
    | 0x17: read_write_multiple_registers
    | 0x18: read_fifo_queue
    | 0x2B: encapsulated_interface_transport

  data = switch function_code {
    case read_coil_status: 
      coil_status: bit[1..2000]
    case read_input_status: 
      input_status: bit[1..2000]
    case read_holding_registers: 
      register_values: uint16[1..125]
    case read_input_registers: 
      register_values: uint16[1..125]
    case write_single_coil: 
      coil_address: uint16
      coil_value: bit
    case write_single_holding_register: 
      register_address: uint16
      register_value: uint16
    case write_multiple_coils: 
      coil_addresses: uint16[1..196]
      coil_values: bit[1..196]
    case write_multiple_holding_registers: 
      register_addresses: uint16[1..125]
      register_values: uint16[1..125]
    case report_slave_id: 
      slave_id: uint8
      additional_info: byte[1..254]
    case read_file_record: 
      file_record_data: byte[1..252]
    case write_file_record: 
      file_record_data: byte[1..252]
    case mask_write_register: 
      register_address: uint16
      and_mask: uint16
      or_mask: uint16
    case read_write_multiple_registers: 
      register_addresses: uint16[1..125]
      register_values: uint16[1..125]
    case read_fifo_queue: 
      fifo_queue_data: byte[1..252]
    case encapsulated_interface_transport: 
      mei_type: uint8
      mei_data: byte[1..252]
  }

  error_code = 
    0x01: illegal_function
    | 0x02: illegal_data_address
    | 0x03: illegal_data_value
    | 0x04: slave_device_failure
    | 0x05: acknowledge
    | 0x06: slave_device_busy
    | 0x07: negative_acknowledge
    | 0x08: memory_parity_error
    | 0x0A: gateway_path_unavailable
    | 0x0B: gateway_target_device_failed_to_respond

  crc = calculate_crc(address, function_code, data)

def modbus_exception_response(address: uint8, function_code: uint8, error_code: uint8, crc: uint16):
  address = range(0, 247)
  function_code = 
    0x01: read_coil_status
    | 0x02: read_input_status
    | 0x03: read_holding_registers
    | 0x04: read_input_registers
    | 0x05: write_single_coil
    | 0x06: write_single_holding_register
    | 0x0F: write_multiple_coils
    | 0x10: write_multiple_holding_registers
    | 0x11: report_slave_id
    | 0x14: read_file_record
    | 0x15: write_file_record
    | 0x16: mask_write_register
    | 0x17: read_write_multiple_registers
    | 0x18: read_fifo_queue
    | 0x2B: encapsulated_interface_transport

  error_code = 
    0x01: illegal_function
    | 0x02: illegal_data_address
    | 0x03: illegal_data_value
    | 0x04: slave_device_failure
    | 0x05: acknowledge
    | 0x06: slave_device_busy
    | 0x07: negative_acknowledge
    | 0x08: memory_parity_error
    | 0x0A: gateway_path_unavailable
    | 0x0B: gateway_target_device_failed_to_respond

  crc = calculate_crc(address, function_code, error_code)