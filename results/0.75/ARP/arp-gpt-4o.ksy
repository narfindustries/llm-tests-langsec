meta:
  id: arp_packet
  title: ARP Packet
  endian: be
seq:
  - id: htype
    type: u2
  - id: ptype
    type: u2
  - id: hlen
    type: u1
  - id: plen
    type: u1
  - id: oper
    type: u2
  - id: sha
    size: hlen
  - id: spa
    size: plen
  - id: tha
    size: hlen
  - id: tpa
    size: plen
enums:
  hardware_type:
    1: ethernet_10mb
  protocol_type:
    0x0800: ipv4
  operation:
    1: arp_request
    2: arp_reply
    3: rarp_request
    4: rarp_reply