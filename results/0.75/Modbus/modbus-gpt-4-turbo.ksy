meta:
  id: modbus
  title: Modbus Protocol
  file-extension: modbus
  endian: le
  license: CC0-1.0
doc: |
  Modbus is a serial communications protocol originally published by Modicon (now Schneider Electric) in 1979 for use with its programmable logic controllers (PLCs). Modbus has become a de facto standard communication protocol and is now a commonly available means of connecting industrial electronic devices.

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
    size: length - 2
    type:
      switch-on: function_code
      cases:
        1: read_coils_resp
        3: read_holding_registers_resp

types:
  read_coils_resp:
    seq:
      - id: byte_count
        type: u1
      - id: coil_status
        type: b1
        repeat: expr
        repeat-expr: 8 * byte_count

  read_holding_registers_resp:
    seq:
      - id: byte_count
        type: u1
      - id: register_values
        type: u2
        repeat: expr
        repeat-expr: byte_count / 2