meta:
  id: arp
  title: Address Resolution Protocol
  endianness: le
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
    type: ip_address
  - id: target_mac
    type: mac_address
  - id: target_ip
    type: ip_address
types:
  mac_address:
    seq:
      - id: bytes
        type: u1
        repeat: 6
  ip_address:
    seq:
      - id: bytes
        type: u1
        repeat: 4