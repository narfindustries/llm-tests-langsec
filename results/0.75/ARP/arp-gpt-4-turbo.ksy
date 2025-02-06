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
    doc: Protocol type (e.g., IP = 0x0800)
  - id: hlen
    type: u1
    doc: Hardware address length (e.g., Ethernet MAC address length = 6)
  - id: plen
    type: u1
    doc: Protocol address length (e.g., IP address length = 4)
  - id: oper
    type: u2
    enum: operation
    doc: ARP operation (e.g., request = 1, reply = 2)
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

enums:
  operation:
    1: request
    2: reply
    3: rarp_request
    4: rarp_reply
    5: drarp_request
    6: drarp_reply
    7: drarp_error
    8: inarp_request
    9: inarp_reply