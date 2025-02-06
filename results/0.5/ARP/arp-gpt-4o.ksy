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
    doc: Operation

  - id: sha
    size: hlen
    type: str
    encoding: ASCII
    doc: Sender hardware address

  - id: spa
    size: plen
    type: str
    encoding: ASCII
    doc: Sender protocol address

  - id: tha
    size: hlen
    type: str
    encoding: ASCII
    doc: Target hardware address

  - id: tpa
    size: plen
    type: str
    encoding: ASCII
    doc: Target protocol address