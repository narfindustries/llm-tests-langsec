meta:
  id: arp_packet
  title: ARP (Address Resolution Protocol) Packet
  license: CC0-1.0
  endian: be

doc: |
  The ARP (Address Resolution Protocol) is used for resolution of network layer addresses into link layer addresses, a critical function in the Internet protocol suite. ARP was defined in 1982 by RFC 826.

seq:
  - id: hw_type
    type: u2
    enum: hardware_type
    doc: Hardware type (HTYPE)
  - id: proto_type
    type: u2
    enum: protocol_type
    doc: Protocol type (PTYPE)
  - id: hw_len
    type: u1
    doc: Length of a hardware address (MAC)
  - id: proto_len
    type: u1
    doc: Length of addresses used in the upper layer protocol
  - id: opcode
    type: u2
    enum: operation
    doc: Specifies the operation that the sender is performing
  - id: sender_hw_addr
    size: hw_len
    doc: MAC address of the sender
  - id: sender_proto_addr
    size: proto_len
    doc: Protocol address of the sender
  - id: target_hw_addr
    size: hw_len
    doc: MAC address of the target
  - id: target_proto_addr
    size: proto_len
    doc: Protocol address of the target

enums:
  hardware_type:
    0x0001: ethernet
    0x0006: ieee_802
    0x000f: fddi

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