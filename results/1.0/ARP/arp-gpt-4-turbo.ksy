meta:
  id: arp_packet
  title: ARP (Address Resolution Protocol)
  endian: be
  xref:
    rfc: 826

seq:
  - id: hrd
    type: u2
    enum: hardware_type
    doc: Hardware type code

  - id: pro
    type: u2
    enum: protocol_type
    doc: Protocol type code

  - id: hln
    type: u1
    doc: Hardware address length

  - id: pln
    type: u1
    doc: Protocol address length

  - id: op
    type: u2
    enum: operation
    doc: ARP operation code

  - id: sha
    size: hln
    doc: Sender hardware address (MAC)

  - id: spa
    size: pln
    doc: Sender protocol address

  - id: tha
    size: hln
    doc: Target hardware address (MAC)

  - id: tpa
    size: pln
    doc: Target protocol address

enums:
  hardware_type:
    1: ethernet
    6: ieee_802
    7: arcnet
    15: frame_relay

  protocol_type:
    0x0800: ipv4

  operation:
    1: request
    2: reply
    3: rarp_request
    4: rarp_reply
