modbus_pdu:
  id: modbus_pdu
  endian: be
  seq:
    - id: transaction_identifier
      type: u2
    - id: protocol_identifier
      type: u2
      default: 0
    - id: length
      type: u2
    - id: unit_identifier
      type: u1
    - id: function_code
      type: u1
    - id: data
      type: modbus_data
      if: function_code < 128
    - id: exception_code
      type: u1
      if: function_code >= 128

modbus_data:
  id: modbus_data
  switch: function_code
  cases:
    1:
      type: read_coils_response
    2:
      type: read_discrete_inputs_response
    3:
      type: read_holding_registers_response
    4:
      type: read_input_registers_response
    5:
      type: write_single_coil_response
    6:
      type: write_single_register_response
    15:
      type: write_multiple_coils_response
    16:
      type: write_multiple_registers_response
    # Add other function codes as needed...


read_coils_response:
  id: read_coils_response
  seq:
    - id: byte_count
      type: u1
    - id: coil_status
      type: bits
      repeat: expr
      repeat-expr: byte_count * 8


read_discrete_inputs_response:
  id: read_discrete_inputs_response
  seq:
    - id: byte_count
      type: u1
    - id: input_status
      type: bits
      repeat: expr
      repeat-expr: byte_count * 8


read_holding_registers_response:
  id: read_holding_registers_response
  seq:
    - id: byte_count
      type: u1
    - id: register_values
      type: u2
      repeat: expr
      repeat-expr: byte_count / 2


read_input_registers_response:
  id: read_input_registers_response
  seq:
    - id: byte_count
      type: u1
    - id: register_values
      type: u2
      repeat: expr
      repeat-expr: byte_count / 2


write_single_coil_response:
  id: write_single_coil_response
  seq:
    - id: starting_address
      type: u2
    - id: coil_value
      type: u2


write_single_register_response:
  id: write_single_register_response
  seq:
    - id: starting_address
      type: u2
    - id: register_value
      type: u2


write_multiple_coils_response:
  id: write_multiple_coils_response
  seq:
    - id: starting_address
      type: u2
    - id: quantity_of_coils
      type: u2


write_multiple_registers_response:
  id: write_multiple_registers_response
  seq:
    - id: starting_address
      type: u2
    - id: quantity_of_registers
      type: u2

