meta:
  id: modbus
  title: Modbus Protocol
  endian: be
seq:
  - id: slave_id
    type: u1
  - id: function_code
    type: u1
  - id: data
    type:
      switch-on: function_code
      cases:
        1: coil_status_data
        2: input_status_data
        3: holding_registers_data
        4: input_registers_data
        5: single_coil_data
        6: single_holding_register_data
        15: multiple_coils_data
        16: multiple_holding_registers_data
        17: report_slave_id_data
        20: file_record_data
        21: file_record_data
        22: mask_write_register_data
        23: read_write_multiple_registers_data
        24: fifo_queue_data
        43: encapsulated_interface_transport_data
types:
  coil_status_data:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_coils
        type: u2
  input_status_data:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_inputs
        type: u2
  holding_registers_data:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_registers
        type: u2
  input_registers_data:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_registers
        type: u2
  single_coil_data:
    seq:
      - id: output_address
        type: u2
      - id: output_value
        type: u2
  single_holding_register_data:
    seq:
      - id: register_address
        type: u2
      - id: register_value
        type: u2
  multiple_coils_data:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_outputs
        type: u2
      - id: output_values
        type: bytes
        size: ((quantity_of_outputs + 7) // 8)
  multiple_holding_registers_data:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_registers
        type: u2
      - id: register_values
        type: u2
        repeat: expr
        repeat-expr: quantity_of_registers
  report_slave_id_data:
    seq:
      - id: slave_id
        type: u1
      - id: run_indicator_status
        type: u1
      - id: additional_info
        type: bytes
        size: 2
  file_record_data:
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
        type: bytes
        size: record_length
  mask_write_register_data:
    seq:
      - id: register_address
        type: u2
      - id: and_mask
        type: u2
      - id: or_mask
        type: u2
  read_write_multiple_registers_data:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_registers
        type: u2
      - id: write_starting_address
        type: u2
      - id: quantity_of_registers_to_write
        type: u2
      - id: write_register_values
        type: u2
        repeat: expr
        repeat-expr: quantity_of_registers_to_write
  fifo_queue_data:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_registers
        type: u2
  encapsulated_interface_transport_data:
    seq:
      - id: mei_type
        type: u1
      - id: mei_data
        type: bytes
        size: 2
  exception_response:
    seq:
      - id: function_code
        type: u1
      - id: exception_code
        type: u1
        enum:
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
  crc:
    seq:
      - id: crc
        type: u2