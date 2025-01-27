meta:
  id: modbus_packet
  title: Modbus Packet
  application: Modbus
  file-extension: None
  endian: be
  encoding: ASCII

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
  - id: payload
    size: length - 2
    type:
      switch-on: function_code
      cases:
        '3': function_read_holding_registers_response
        '6': function_preset_single_register_request

types:
  function_read_holding_registers_response:
    seq:
      - id: byte_count
        type: u1
      - id: register_values
        type: u2
        repeat: expr
        repeat-expr: byte_count / 2

  function_preset_single_register_request:
    seq:
      - id: address
        type: u2
      - id: value
        type: u2