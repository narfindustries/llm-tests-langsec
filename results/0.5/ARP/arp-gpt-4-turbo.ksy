meta:
  id: arp_packet
  title: ARP (Address Resolution Protocol) Packet
  license: CC0-1.0
  endian: be

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
    size: hlen
    doc: Sender hardware address
  - id: spa
    size: plen
    doc: Sender protocol address
  - id: tha
    size: hlen
    doc: Target hardware address
  - id: tpa
    size: plen
    doc: Target protocol address

doc: |
  The ARP packet structure according to RFC 826. This structure is used
  for resolving network layer addresses into link layer addresses in
  networks such as Ethernet.