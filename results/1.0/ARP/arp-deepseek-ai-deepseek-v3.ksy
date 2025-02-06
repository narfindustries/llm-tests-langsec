meta:
  id: arp
  title: ARP (Address Resolution Protocol)
  file-extension: arp
  license: CC0-1.0
  endian: be

seq:
  - id: htype
    type: u2
    doc: Hardware Type (e.g., Ethernet = 1)
  - id: ptype
    type: u2
    doc: Protocol Type (e.g., IPv4 = 0x0800)
  - id: hlen
    type: u1
    doc: Hardware Address Length (e.g., 6 for Ethernet MAC)
  - id: plen
    type: u1
    doc: Protocol Address Length (e.g., 4 for IPv4)
  - id: oper
    type: u2
    doc: Operation (e.g., 1 = ARP Request, 2 = ARP Reply)
  - id: sha
    size: hlen
    type: str
    encoding: ISO-8859-1
    doc: Sender Hardware Address
  - id: spa
    size: plen
    type: str
    encoding: ISO-8859-1
    doc: Sender Protocol Address
  - id: tha
    size: hlen
    type: str
    encoding: ISO-8859-1
    doc: Target Hardware Address
  - id: tpa
    size: plen
    type: str
    encoding: ISO-8859-1
    doc: Target Protocol Address

enums:
  htype:
    1: ethernet
  ptype:
    0x0800: ipv4
  oper:
    1: arp_request
    2: arp_reply
    3: rarp_request
    4: rarp_reply