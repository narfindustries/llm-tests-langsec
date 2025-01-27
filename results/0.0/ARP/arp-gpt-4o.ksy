meta:
  id: arp_packet
  title: ARP Packet
  application: Address Resolution Protocol
  file-extension: arp
  endian: be

seq:
  - id: hardware_type
    type: u2
  - id: protocol_type
    type: u2
  - id: hardware_size
    type: u1
  - id: protocol_size
    type: u1
  - id: opcode
    type: u2
  - id: sender_mac
    type: mac_address
  - id: sender_ip
    type: ipv4_address
  - id: target_mac
    type: mac_address
  - id: target_ip
    type: ipv4_address

types:
  mac_address:
    seq:
      - id: octets
        type: u1
        repeat: expr
        repeat-expr: 6

  ipv4_address:
    seq:
      - id: octets
        type: u1
        repeat: expr
        repeat-expr: 4