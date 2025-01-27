meta:
  id: modbus
  title: Modbus
  endian: le

types:
  uint8: u1
  uint16: u2
  uint32: u4
  uint64: u8
  int8: s1
  int16: s2
  int32: s4
  int64: s8
  float16: f2
  float32: f4
  float64: f8

seq:
  - id: transaction_id
    type: uint16
  - id: protocol_id
    type: uint16
  - id: length
    type: uint16
  - id: unit_id
    type: uint8
  - id: function_code
    type: uint8
  - id: payload
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
        0x2b: read_device_id

types:
  read_coils:
    seq:
      - id: starting_address
        type: uint16
      - id: quantity
        type: uint16

  read_discrete_inputs:
    seq:
      - id: starting_address
        type: uint16
      - id: quantity
        type: uint16

  read_holding_registers:
    seq:
      - id: starting_address
        type: uint16
      - id: quantity
        type: uint16

  read_input_registers:
    seq:
      - id: starting_address
        type: uint16
      - id: quantity
        type: uint16

  write_single_coil:
    seq:
      - id: output_address
        type: uint16
      - id: output_value
        type: uint16

  write_single_register:
    seq:
      - id: register_address
        type: uint16
      - id: register_value
        type: uint16

  write_multiple_coils:
    seq:
      - id: starting_address
        type: uint16
      - id: quantity
        type: uint16
      - id: output_values
        type: bytes
        size: (quantity + 7) // 8

  write_multiple_registers:
    seq:
      - id: starting_address
        type: uint16
      - id: quantity
        type: uint16
      - id: register_values
        type: bytes
        size: quantity * 2

  read_device_id:
    seq:
      - id: read_device_id
        type: uint8
      - id: object_id
        type: uint8