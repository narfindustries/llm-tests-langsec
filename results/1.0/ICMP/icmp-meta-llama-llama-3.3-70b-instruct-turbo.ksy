meta:
  endian: le
seq:
  - id: type
    type: u1
  - id: code
    type: u1
  - id: checksum
    type: u2
  - id: identifier
    type: u2
  - id: sequence_number
    type: u2
instances:
  destination_unreachable:
    seq:
      - id: unused
        type: u1
      - id: next_hop_mtu
        type: u2
    size: eos
    if: type == 3
  echo:
    seq:
      - id: header_data
        type: echo_header
    if: type == 8 or type == 0
  redirect:
    seq:
      - id: gateway_address
        type: u4
    size: eos
    if: type == 5
  source_quench:
    seq:
      - id: unused
        type: u1
    size: eos
    if: type == 4
  time_exceeded:
    seq:
      - id: unused
        type: u1
    size: eos
    if: type == 11
  information_request:
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2
    size: eos
    if: type == 15
  information_reply:
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2
    size: eos
    if: type == 16
  traceroute:
    seq:
      - id: unused
        type: u1
    size: eos
    if: type == 30
types:
  u1:
    type: uint8
  u2:
    type: uint16
  u4:
    type: uint32
  str:
    type: string
  echo_header:
    seq:
      - id: timestamp
        type: u4
      - id: data
        type: str
        size: eos