meta:
  id: modbus
  title: Modbus Protocol
  endian: be

seq:
  - id: slave_address
    type: u1
    doc: Slave address (1-247)

  - id: function_code
    type: u1
    doc: Function code

  - id: data
    size-eos: true
    type: data_field
    doc: Data field, varies by function code

  - id: crc
    type: u2
    doc: CRC for error checking (Modbus RTU)

types:
  data_field:
    seq:
      - id: starting_address
        type: u2
        doc: Starting address for the operation

      - id: quantity
        type: u2
        doc: Quantity of coils/registers

      - id: values
        size-eos: true
        doc: Data values, length depends on function code

enums:
  function_code:
    0x01: read_coils
    0x02: read_discrete_inputs
    0x03: read_holding_registers
    0x04: read_input_registers
    0x05: write_single_coil
    0x06: write_single_register
    0x0F: write_multiple_coils
    0x10: write_multiple_registers
    0x81: exception_read_coils
    0x82: exception_read_discrete_inputs
    0x83: exception_read_holding_registers
    0x84: exception_read_input_registers
    0x85: exception_write_single_coil
    0x86: exception_write_single_register
    0x8F: exception_write_multiple_coils
    0x90: exception_write_multiple_registers

  exception_code:
    0x01: illegal_function
    0x02: illegal_data_address
    0x03: illegal_data_value
    0x04: slave_device_failure