modbus_message = 
  address: uint8,
  function_code: uint8,
  data: 
    read_coil_status: 
      starting_address: uint16,
      quantity_of_coils: uint16
    read_input_status: 
      starting_address: uint16,
      quantity_of_inputs: uint16
    read_holding_registers: 
      starting_address: uint16,
      quantity_of_registers: uint16
    read_input_registers: 
      starting_address: uint16,
      quantity_of_registers: uint16
    write_single_coil: 
      output_address: uint16,
      output_value: uint16
    write_single_holding_register: 
      register_address: uint16,
      register_value: uint16
    write_multiple_coils: 
      starting_address: uint16,
      quantity_of_outputs: uint16,
      output_values: array (uint16) of size quantity_of_outputs
    write_multiple_holding_registers: 
      starting_address: uint16,
      quantity_of_registers: uint16,
      register_values: array (uint16) of size quantity_of_registers
    report_slave_id: 
      slave_id: uint8,
      run_indicator_status: uint8,
      additional_info: string (optional)
    read_file_record: 
      file_number: uint16,
      record_number: uint16,
      record_length: uint16
    write_file_record: 
      file_number: uint16,
      record_number: uint16,
      record_data: array (uint8) of size record_length
    mask_write_register: 
      register_address: uint16,
      and_mask: uint16,
      or_mask: uint16
    read_write_multiple_registers: 
      read_starting_address: uint16,
      quantity_of_registers_to_read: uint16,
      write_starting_address: uint16,
      quantity_of_registers_to_write: uint16,
      write_register_values: array (uint16) of size quantity_of_registers_to_write
    read_fifo_queue: 
      fifo_pointer_address: uint16
    encapsulated_interface_transport: 
      interface_reference_type: uint16,
      interface_reference_number: uint16,
      interface_specific_data: array (uint8) of size interface_specific_data_length
    exception_response: 
      error_code: uint8,
  crc: uint16 (optional)