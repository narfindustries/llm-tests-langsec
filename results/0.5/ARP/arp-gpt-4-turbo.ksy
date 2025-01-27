meta:
  id: arp_packet
  title: ARP (Address Resolution Protocol)
  license: CC0-1.0
  endian: be

doc: |
  The Address Resolution Protocol (ARP) is a network protocol used to find out the address of a network node from its IP address. ARP is used for mapping an IP address to a physical machine address that is recognized in the local network.

seq:
  - id: hw_type
    type: u2
    enum: hardware_type
    doc: Type of hardware (e.g., Ethernet = 1)

  - id: proto_type
    type: u2
    enum: protocol_type
    doc: Type of protocol (e.g., IP = 0x0800)

  - id: hw_size
    type: u1
    doc: Length of hardware address (Ethernet addresses size is 6)

  - id: proto_size
    type: u1
    doc: Length of protocol address (IP address size is 4)

  - id: opcode
    type: u2
    enum: operation
    doc: ARP opcode (command)

  - id: src_hw_addr
    size: 6
    doc: Sender hardware address (MAC)

  - id: src_proto_addr
    size: 4
    doc: Sender protocol address (IPv4)

  - id: dst_hw_addr
    size: 6
    doc: Target hardware address (MAC)

  - id: dst_proto_addr
    size: 4
    doc: Target protocol address (IPv4)

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
    0x86dd: ipv6

  operation:
    1: request
    2: reply
    3: request_reverse
    4: reply_reverse