meta:
  id: arp_packet
  title: ARP Packet
  application: Address Resolution Protocol
  license: CC0-1.0
  endian: be

seq:
  - id: htype
    type: u2
    doc: Hardware type

  - id: ptype
    type: u2
    doc: Protocol type

  - id: hlen
    type: u1
    doc: Hardware address length

  - id: plen
    type: u1
    doc: Protocol address length

  - id: oper
    type: u2
    doc: Operation code
    enum: operation

  - id: sha
    type: str
    encoding: ASCII
    size: hlen
    doc: Sender hardware address

  - id: spa
    type: str
    encoding: ASCII
    size: plen
    doc: Sender protocol address

  - id: tha
    type: str
    encoding: ASCII
    size: hlen
    doc: Target hardware address

  - id: tpa
    type: str
    encoding: ASCII
    size: plen
    doc: Target protocol address

enums:
  operation:
    1: arp_request
    2: arp_reply
    3: rarp_request
    4: rarp_reply