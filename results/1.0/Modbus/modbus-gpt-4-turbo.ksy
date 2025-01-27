meta:
  id: modbus
  title: Modbus Protocol
  file-extension: modbus
  endian: le
  license: CC0-1.0
doc: |
  The Modbus protocol is a communication protocol based on a request/reply technique
  employed for industrial automation systems.

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
    type: pdu_packet

types:
  pdu_packet:
    seq:
      - id: func_code
        type: u1
      - id: data
        size: "(length - 2)"
      - id: error_check
        type: u2
        if: func_code == 0x01