meta:
  id: arp_packet
  title: ARP (Address Resolution Protocol) Packet
  license: CC0-1.0
  endian: be

doc: |
  The Address Resolution Protocol (ARP) is a network layer protocol
  used for mapping an IP address to a physical machine address that is
  recognized in the local network.

seq:
  - id: htype
    type: u2
    doc: Hardware type (e.g., Ethernet = 1)
  - id: ptype
    type: u2
    doc: Protocol type (e.g., IPv4 = 0x0800)
  - id: hlen
    type: u1
    doc: Hardware address length (e.g., Ethernet = 6)
  - id: plen
    type: u1
    doc: Protocol address length (e.g., IPv4 = 4)
  - id: oper
    type: u2
    doc: Operation (1 = request, 2 = reply)
  - id: sha
    size: sha_size
    doc: Sender hardware address
  - id: spa
    size: spa_size
    doc: Sender protocol address
  - id: tha
    size: tha_size
    doc: Target hardware address
  - id: tpa
    size: tpa_size
    doc: Target protocol address

instances:
  sha_size:
    value: hlen
  spa_size:
    value: plen
  tha_size:
    value: hlen
  tpa_size:
    value: plen