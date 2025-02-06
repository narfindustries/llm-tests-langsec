meta:
  id: icmp_packet
  title: ICMP Packet
  application: internet
  endian: be

seq:
  - id: type
    type: u1
  - id: code
    type: u1
  - id: checksum
    type: u2
  - id: rest_of_header
    type: rest_of_header
  - id: data
    size-eos: true

types:
  rest_of_header:
    seq:
      - id: unused
        type: u4
        if: _parent.type == 3 or _parent.type == 11 or _parent.type == 12
      - id: gateway_internet_address
        type: ipv4_address
        if: _parent.type == 5
      - id: echo
        type: echo
        if: _parent.type == 0 or _parent.type == 8

  echo:
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2

  ipv4_address:
    seq:
      - id: b1
        type: u1
      - id: b2
        type: u1
      - id: b3
        type: u1
      - id: b4
        type: u1
    instances:
      address:
        value: 'b1.to_s + "." + b2.to_s + "." + b3.to_s + "." + b4.to_s'