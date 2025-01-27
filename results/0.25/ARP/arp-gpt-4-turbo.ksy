meta:
  id: arp_packet
  title: ARP (Address Resolution Protocol) Packet
  license: CC0-1.0
  endian: be

doc: |
  The Address Resolution Protocol (ARP) is a network protocol used to find out
  the address of a network node from its Internet address.

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
    doc: Hardware size (e.g., Ethernet = 6)

  - id: proto_size
    type: u1
    doc: Protocol size (e.g., IPv4 = 4)

  - id: opcode
    type: u2
    enum: opcode
    doc: ARP opcode (command)

  - id: src_hw_addr
    size: src_hw_addr_size
    doc: Source hardware address (MAC)

  - id: src_proto_addr
    size: src_proto_addr_size
    doc: Source protocol address (IP)

  - id: dst_hw_addr
    size: dst_hw_addr_size
    doc: Destination hardware address (MAC)

  - id: dst_proto_addr
    size: dst_proto_addr_size
    doc: Destination protocol address (IP)

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
    17: hdlc
    18: fibre_channel
    19: atm_rfc2225
    20: serial_line

  protocol_type:
    0x0800: ipv4
    0x0806: arp
    0x8035: rarp
    0x86DD: ipv6

  opcode:
    1: request
    2: reply
    3: request_reverse
    4: reply_reverse