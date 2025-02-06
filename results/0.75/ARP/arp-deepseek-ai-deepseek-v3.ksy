meta:
  id: arp
  title: ARP (Address Resolution Protocol)
  file-extension: arp
  license: MIT
  endian: be
seq:
  - id: htype
    type: u2
    doc: Hardware type (e.g., 1 for Ethernet)
  - id: ptype
    type: u2
    doc: Protocol type (e.g., 0x0800 for IPv4)
  - id: hlen
    type: u1
    doc: Hardware address length (e.g., 6 for Ethernet)
  - id: plen
    type: u1
    doc: Protocol address length (e.g., 4 for IPv4)
  - id: oper
    type: u2
    doc: Operation (e.g., 1 for Request, 2 for Reply)
  - id: sha
    size: hlen
    doc: Sender hardware address (e.g., MAC address)
  - id: spa
    size: plen
    doc: Sender protocol address (e.g., IPv4 address)
  - id: tha
    size: hlen
    doc: Target hardware address (e.g., MAC address)
  - id: tpa
    size: plen
    doc: Target protocol address (e.g., IPv4 address)