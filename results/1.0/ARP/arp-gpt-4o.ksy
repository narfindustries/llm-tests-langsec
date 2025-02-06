meta:
  id: arp_packet
  title: ARP Packet
  description: Packet structure of the Address Resolution Protocol (ARP)
  license: CC0-1.0
  endian: be

seq:
  - id: htype
    type: u2
    doc: Hardware type (e.g., Ethernet)
  
  - id: ptype
    type: u2
    doc: Protocol type (e.g., IPv4)
  
  - id: hlen
    type: u1
    doc: Hardware address length (e.g., 6 for Ethernet)
  
  - id: plen
    type: u1
    doc: Protocol address length (e.g., 4 for IPv4)
  
  - id: oper
    type: u2
    doc: Operation (e.g., ARP request or reply)
  
  - id: sha
    type: bytes
    size: hlen
    doc: Sender hardware address
  
  - id: spa
    type: bytes
    size: plen
    doc: Sender protocol address
  
  - id: tha
    type: bytes
    size: hlen
    doc: Target hardware address
  
  - id: tpa
    type: bytes
    size: plen
    doc: Target protocol address

instances:
  hardware_type_description:
    value: 'Hardware type is Ethernet' if htype == 1 else 'Hardware type is unknown'

  protocol_type_description:
    value: 'Protocol type is IPv4' if ptype == 0x0800 else 'Protocol type is unknown'

  operation_description:
    value:
      _switch: oper
      cases:
        1: ARP Request
        2: ARP Reply
        3: RARP Request
        4: RARP Reply
        _: Unknown operation