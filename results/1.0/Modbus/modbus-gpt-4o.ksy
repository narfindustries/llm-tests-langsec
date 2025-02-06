meta:
  id: modbus
  title: Modbus Protocol
  license: CC0-1.0
  endian: be

types:
  adu:
    seq:
      - id: address
        type: u1
      - id: pdu
        type: pdu
      - id: error_check
        type:
          switch-on: _root.mode
          cases:
            rtu: u2
            ascii: u1
        if: _root.mode != tcp

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
            0x0f: write_multiple_coils
            0x10: write_multiple_registers
            _: generic_data

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
      - id: quantity_of_inputs
        type: u2

  write_single_coil:
    seq:
      - id: address
        type: u2
      - id: value
        type: u2

  write_single_register:
    seq:
      - id: address
        type: u2
      - id: value
        type: u2

  write_multiple_coils:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_outputs
        type: u2
      - id: byte_count
        type: u1
      - id: output_values
        type: bytes
        size: byte_count

  write_multiple_registers:
    seq:
      - id: starting_address
        type: u2
      - id: quantity_of_registers
        type: u2
      - id: byte_count
        type: u1
      - id: register_values
        type: bytes
        size: byte_count

  generic_data:
    seq:
      - id: data
        type: bytes
        size-eos: true

  mbap_header:
    seq:
      - id: transaction_id
        type: u2
      - id: protocol_id
        type: u2
      - id: length
        type: u2
      - id: unit_id
        type: u1

seq:
  - id: mbap
    type: mbap_header
    if: _root.mode == tcp
  - id: adu_frame
    type: adu

params:
  mode:
    type: mode_enum

enums:
  mode_enum:
    rtu: 0
    ascii: 1
    tcp: 2