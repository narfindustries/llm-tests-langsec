meta:
  id: modbus
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
  - id: data
    type:
      switch-on: function_code
      cases:
        1: read_coils
        2: read_discrete_inputs
        3: read_holding_registers
        4: read_input_registers
        5: write_single_coil
        6: write_single_register
        15: write_multiple_coils
        16: write_multiple_registers
        22: mask_write_register
        23: read_write_multiple_registers
        24: read_fifo_queue
        43: encapsulated_interface_transport
types:
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
  write_multiple_coils:
    seq:
      - id: starting_address
        type: u2
      - id: num_coils
        type: u2
      - id: byte_count
        type: u1
      - id: coils
        type: u1
        repeat: expr
        repeat-expr: byte_count
  write_multiple_registers:
    seq:
      - id: starting_address
        type: u2
      - id: num_registers
        type: u2
      - id: byte_count
        type: u1
      - id: registers
        type: u2
        repeat: expr
        repeat-expr: num_registers
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
      - id: read_quantity
        type: u2
      - id: write_starting_address
        type: u2
      - id: num_write_registers
        type: u2
      - id: write_byte_count
        type: u1
      - id: write_registers
        type: u2
        repeat: expr
        repeat-expr: num_write_registers
  read_fifo_queue:
    seq:
      - id: fifo_pointer_address
        type: u2
  encapsulated_interface_transport:
    seq:
      - id: mei_type
        type: u1
      - id: device_id_code
        type: u1
      - id: object_id
        type: u1
      - id: object_value
        type: u1
  exception_response:
    seq:
      - id: exception_function_code
        type: u1
      - id: exception_code
        type: u1
        enum: exception_codes
enums:
  exception_codes:
    1: illegal_function
    2: illegal_data_address
    3: illegal_data_value
    4: slave_device_failure
    5: acknowledge
    6: slave_device_busy
    7: negative_acknowledge
    8: memory_parity_error
    10: gateway_path_unavailable
    11: gateway_target_device_failed_to_respond