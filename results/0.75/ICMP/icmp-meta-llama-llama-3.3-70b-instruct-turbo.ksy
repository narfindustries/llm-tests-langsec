meta:
  id: icmp
  endian: be
seq:
  - id: type
    type: u1
  - id: code
    type: u1
  - id: checksum
    type: u2
  - id: identifier
    type: u2
    if: type == 8 or type == 0
  - id: sequence_number
    type: u2
    if: type == 8 or type == 0
  - id: gateway_address
    type: u4
    if: type == 5
  - id: mtu
    type: u2
    if: code == 4 and type == 3
  - id: pointer
    type: u1
    if: type == 12
  - id: timestamp
    type: u4
    if: type == 13 or type == 14
  - id: data
    type: str
    encoding: ascii
    size: eos
    if: type == 8 or type == 0 or type == 11 or type == 3