meta:
  id: arp
  title: ARP (Address Resolution Protocol)
  license: CC0-1.0
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
    doc: Operation (1 for ARP request, 2 for ARP reply)
  - id: sha
    size: hlen
    doc: Sender hardware address (MAC address)
  - id: spa
    size: plen
    doc: Sender protocol address (IP address)
  - id: tha
    size: hlen
    doc: Target hardware address (MAC address)
  - id: tpa
    size: plen
    doc: Target protocol address (IP address)