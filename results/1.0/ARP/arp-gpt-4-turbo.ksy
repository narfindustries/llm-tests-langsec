meta:
  id: arp_packet
  title: ARP (Address Resolution Protocol) Packet
  license: CC0-1.0
  endian: be

doc: |
  The Address Resolution Protocol (ARP) is a network protocol used to find out
  the hardware (MAC) address of a device from an IP address.

seq:
  - id: htype
    type: u2
    doc: Hardware type (Ethernet is 1)
  - id: ptype
    type: u2
    doc: Protocol type (IPv4 is 0x0800)
  - id: hlen
    type: u1
    doc: Hardware address length (Ethernet addresses size is 6)
  - id: plen
    type: u1
    doc: Protocol address length (IPv4 address size is 4)
  - id: oper
    type: u2
    enum: operation
    doc: Operation (1 for request, 2 for reply)
  - id: sha
    size: 6
    doc: Sender hardware address (MAC)
  - id: spa
    size: 4
    doc: Sender protocol address (IPv4)
  - id: tha
    size: 6
    doc: Target hardware address (MAC)
  - id: tpa
    size: 4
    doc: Target protocol address (IPv4)

enums:
  operation:
    1: request
    2: reply