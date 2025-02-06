meta:
  id: modbus
  title: Modbus Protocol
  application: industrial automation
  file-extension: modbus
  endian: be

seq:
  - id: transport
    type:
      switch-on: _io.size
      cases:
        8: modbus_tcp
        6: modbus_rtu

types:
  modbus_tcp:
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

  modbus_rtu:
    seq:
      - id: address
        type: u1
      - id: pdu
        type: pdu
      - id: crc
        type: u2

  pdu:
    seq:
      - id: function_code
        type: u1
      - id: data
        size-eos: true
        type:
          switch-on: function_code
          cases:
            0x01: read_coils
            0x02: read_discrete_inputs
            0x03: read_holding_registers
            0x04: read_input_registers
            0x05: write_single_coil
            0x06: write_single_register
            0x0f: write_multiple_coils
            0x10: write_multiple_registers
            0x16: mask_write_register
            0x17: read_write_multiple_registers
            0x2b: encapsulated_interface_transport

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

  write_multiple_coils:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_outputs
        type: u2
      - id: byte_count
        type: u1
      - id: outputs_value
        size: byte_count

  write_multiple_registers:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_registers
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
      - id: quantity_to_read
        type: u2
      - id: write_starting_address
        type: u2
      - id: quantity_to_write
        type: u2
      - id: write_byte_count
        type: u1
      - id: write_registers_value
        size: write_byte_count

  encapsulated_interface_transport:
    seq:
      - id: mei_type
        type: u1
      - id: mei_data
        size-eos: true