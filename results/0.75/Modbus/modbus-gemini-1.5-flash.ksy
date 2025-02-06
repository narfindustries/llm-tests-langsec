types:
  - id: modbus_read_coils_response
    fields:
      - id: byte_count
        type: u1
      - id: coil_status
        type: bits
        repeat: expr
        expr: byte_count * 8

  - id: modbus_read_input_registers_response
    fields:
      - id: byte_count
        type: u1
      - id: data
        type: u2
        repeat: expr
        expr: byte_count / 2

  - id: modbus_read_holding_registers_response
    fields:
      - id: byte_count
        type: u1
      - id: data
        type: u2
        repeat: expr
        expr: byte_count / 2

  - id: modbus_write_single_coil_response
    fields:
      - id: starting_address
        type: u2
      - id: coil_value
        type: u2

  - id: modbus_write_single_register_response
    fields:
      - id: starting_address
        type: u2
      - id: register_value
        type: u2

  - id: modbus_read_exception_status
    fields:
      - id: exception_code
        type: u1

  - id: modbus_diagnostic_response
    fields:
      - id: sub_function_code
        type: u1
      - id: data
        type: u2
        repeat: expr
        expr: length - 3

- id: modbus_pdu
  fields:
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
      type: seq
      read: function_code_data
      contents:
        - id: read_coils_response
          type: modbus_read_coils_response
          if: function_code == 0x01
        - id: read_input_registers_response
          type: modbus_read_input_registers_response
          if: function_code == 0x04
        - id: read_holding_registers_response
          type: modbus_read_holding_registers_response
          if: function_code == 0x03
        - id: write_single_coil_response
          type: modbus_write_single_coil_response
          if: function_code == 0x01 | 0x0F | 0x10
        - id: write_single_register_response
          type: modbus_write_single_register_response
          if: function_code == 0x06 | 0x10
        - id: read_exception_status
          type: modbus_read_exception_status
          if: function_code == 0x08
        - id: diagnostic_response
          type: modbus_diagnostic_response
          if: function_code == 0x08
        - id: default
          type: bytes
          repeat: expr
          expr: length - 1

