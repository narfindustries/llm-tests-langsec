meta:
  id: icmp_packet
  title: Internet Control Message Protocol (ICMP) Packet
  file-extension: icmp
  license: CC0-1.0
  endian: be

seq:
  - id: type
    type: u1
  - id: code
    type: u1
  - id: checksum
    type: u2
  - id: rest_of_header
    size: 4
  - id: data
    size-eos: true

types:
  icmp_echo:
    doc: ICMP Echo (ping) request or response.
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2
      - id: payload
        type: str
        size-eos: true

  icmp_destination_unreachable:
    doc: ICMP Destination Unreachable message.
    seq:
      - id: unused
        size: 4
      - id: original_datagram
        size-eos: true

  icmp_redirect_message:
    doc: ICMP Redirect message.
    seq:
      - id: gateway_internet_address
        type: ipv4
      - id: original_datagram
        size-eos: true

types:
  ipv4:
    seq:
      - id: octet_1
        type: u1
      - id: octet_2
        type: u1
      - id: octet_3
        type: u1
      - id: octet_4
        type: u1

instances:
  message:
    switch-on: type
    cases:
      0x00: icmp_echo
      0x08: icmp_echo
      0x03: icmp_destination_unreachable
      0x05: icmp_redirect_message