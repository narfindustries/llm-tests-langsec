meta:
  id: arp_packet
  title: ARP (Address Resolution Protocol)
  license: CC0-1.0
  endian: be

doc: |
  The Address Resolution Protocol (ARP) is a network protocol used to find out the address of a network node from its IP address.

seq:
  - id: hw_type
    type: u2
    enum: hardware_type
    doc: Hardware type (e.g., Ethernet = 1)

  - id: proto_type
    type: u2
    enum: protocol_type
    doc: Protocol type (e.g., IPv4 = 0x0800)

  - id: hw_size
    type: u1
    doc: Hardware address length (e.g., Ethernet MAC address length is 6)

  - id: proto_size
    type: u1
    doc: Protocol address length (e.g., IPv4 address length is 4)

  - id: opcode
    type: u2
    enum: operation
    doc: ARP opcode (command)

  - id: src_hw_addr
    size: src_hw_addr_size
    doc: Sender hardware address (MAC)

  - id: src_proto_addr
    size: src_proto_addr_size
    doc: Sender protocol address (IP)

  - id: dst_hw_addr
    size: dst_hw_addr_size
    doc: Target hardware address (MAC)

  - id: dst_proto_addr
    size: dst_proto_addr_size
    doc: Target protocol address (IP)

instances:
  src_hw_addr_size:
    value: hw_size

  src_proto_addr_size:
    value: proto_size

  dst_hw_addr_size:
    value: hw_size

  dst_proto_addr_size:
    value: proto_size

enums:
  hardware_type:
    1: ethernet
    6: ieee_802
    7: arcnet
    15: frame_relay
    16: atm
    17: hdsl
    18: fibre_channel
    19: atm2
    20: serial_line

  protocol_type:
    0x0800: ipv4
    0x86DD: ipv6

  operation:
    1: request
    2: reply
    3: request_reverse
    4: reply_reverse