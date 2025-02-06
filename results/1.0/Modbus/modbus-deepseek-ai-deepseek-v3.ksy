meta:
  id: modbus
  title: Modbus Protocol
  application: protocol
  file-extension: modbus
  license: MIT
  endian: be
seq:
  - id: transaction_id
    type: u2
  - id: protocol_id
    type: u2
  - id: length
    type: u2
  - id: unit_id
    type: u1
  - id: function_code
    type: u1
    enum: function_codes
  - id: data
    type:
      switch-on: function_code
      cases:
        0x01: modbus_data.read_coils
        0x02: modbus_data.read_discrete_inputs
        0x03: modbus_data.read_holding_registers
        0x04: modbus_data.read_input_registers
        0x05: modbus_data.write_single_coil
        0x06: modbus_data.write_single_register
        0x07: modbus_data.read_exception_status
        0x08: modbus_data.diagnostics
        0x0B: modbus_data.get_comm_event_counter
        0x0C: modbus_data.get_comm_event_log
        0x0F: modbus_data.write_multiple_coils
        0x10: modbus_data.write_multiple_registers
        0x11: modbus_data.report_slave_id
        0x14: modbus_data.read_file_record
        0x15: modbus_data.write_file_record
        0x16: modbus_data.mask_write_register
        0x17: modbus_data.read_write_multiple_registers
        0x18: modbus_data.read_fifo_queue
        0x2B: modbus_data.encapsulated_interface_transport
enums:
  function_codes:
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
    0x11: report_slave_id
    0x14: read_file_record
    0x15: write_file_record
    0x16: mask_write_register
    0x17: read_write_multiple_registers
    0x18: read_fifo_queue
    0x2B: encapsulated_interface_transport
types:
  modbus_data:
    read_coils:
      seq:
        - id: starting_address
          type: u2
        - id: quantity
          type: u2
    read_discrete_inputs:
      seq:
        - id: starting_address
          type: u2
        - id: quantity
          type: u2
    read_holding_registers:
      seq:
        - id: starting_address
          type: u2
        - id: quantity
          type: u2
    read_input_registers:
      seq:
        - id: starting_address
          type: u2
        - id: quantity
          type: u2
    write_single_coil:
      seq:
        - id: starting_address
          type: u2
        - id: value
          type: u2
    write_single_register:
      seq:
        - id: starting_address
          type: u2
        - id: value
          type: u2
    read_exception_status:
      seq: []
    diagnostics:
      seq:
        - id: sub_code
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
        - id: quantity
          type: u2
        - id: num_data
          type: u1
        - id: data
          type: u1
          repeat: expr
          repeat-expr: num_data
    write_multiple_registers:
      seq:
        - id: starting_address
          type: u2
        - id: quantity
          type: u2
        - id: num_data
          type: u1
        - id: data
          type: u1
          repeat: expr
          repeat-expr: num_data
    report_slave_id:
      seq: []
    read_file_record:
      seq:
        - id: num_data
          type: u1
        - id: data
          type: u1
          repeat: expr
          repeat-expr: num_data
    write_file_record:
      seq:
        - id: num_data
          type: u1
        - id: data
          type: u1
          repeat: expr
          repeat-expr: num_data
    mask_write_register:
      seq:
        - id: address
          type: u2
        - id: and_mask
          type: u2
        - id: or_mask
          type: u2
    read_write_multiple_registers:
      seq:
        - id: read_starting_address
          type: u2
        - id: read_quantity
          type: u2
        - id: write_starting_address
          type: u2
        - id: write_quantity
          type: u2
        - id: num_write_data
          type: u1
        - id: write_data
          type: u1
          repeat: expr
          repeat-expr: num_write_data
    read_fifo_queue:
      seq:
        - id: fifo_address
          type: u2
    encapsulated_interface_transport:
      seq:
        - id: mei_type
          type: u1
        - id: mei_data
          type: u1
          repeat: eos