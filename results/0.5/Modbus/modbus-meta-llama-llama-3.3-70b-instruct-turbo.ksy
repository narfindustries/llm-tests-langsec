meta:
  id: modbus
  file-extension: bin
  endian: little
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
    type:
      switch-on: function_code
      cases:
        0x01: seq:
          - id: start_addr
            type: u2
          - id: num_coils
            type: u2
        0x02: seq:
          - id: start_addr
            type: u2
          - id: num_coils
            type: u2
        0x03: seq:
          - id: start_addr
            type: u2
          - id: num_registers
            type: u2
        0x04: seq:
          - id: start_addr
            type: u2
          - id: num_registers
            type: u2
        0x05: seq:
          - id: output_addr
            type: u2
          - id: output_value
            type: u2
        0x06: seq:
          - id: register_addr
            type: u2
          - id: register_value
            type: u2
        0x0f: seq:
          - id: start_addr
            type: u2
          - id: num_coils
            type: u2
          - id: coils
            type: bytes
            size: num_coils // 8 + (num_coils % 8 != 0 ? 1 : 0)
        0x10: seq:
          - id: start_addr
            type: u2
          - id: num_registers
            type: u2
          - id: registers
            type: u2
            repeat: num_registers