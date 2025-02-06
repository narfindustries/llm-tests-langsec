meta:
  id: modbus
  title: Modbus Protocol
  application: industrial
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
  - id: pdu
    type: pdu
types:
  pdu:
    seq:
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
            0x0F: write_multiple_coils
            0x10: write_multiple_registers
            0x16: mask_write_register
            0x17: read_write_multiple_registers
            0x18: read_fifo_queue
            0x2B: encapsulated_interface_transport
            0x81: exception_response
            0x82: exception_response
            0x83: exception_response
            0x84: exception_response
            0x85: exception_response
            0x86: exception_response
            0x87: exception_response
            0x88: exception_response
            0x8A: exception_response
            0x8B: exception_response
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
      - id: quantity
        type: u2
      - id: byte_count
        type: u1
      - id: outputs_value
        size: byte_count
  write_multiple_registers:
    seq:
      - id: starting_address
        type: u2
      - id: quantity
        type: u2
      - id: byte_count
        type: u1
      - id: registers_value
        size: byte_count
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
      - id: write_quantity
        type: u2
      - id: write_byte_count
        type: u1
      - id: write_registers_value
        size: write_byte_count
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
  exception_response:
    seq:
      - id: exception_code
        type: u1