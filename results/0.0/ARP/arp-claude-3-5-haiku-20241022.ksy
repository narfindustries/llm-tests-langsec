meta:
  id: arp
  title: Address Resolution Protocol (ARP)
  endian: be
seq:
  - id: hardware_type
    type: u2
    enum: hardware_type_enum
  - id: protocol_type
    type: u2
    enum: protocol_type_enum
  - id: hardware_size
    type: u1
  - id: protocol_size
    type: u1
  - id: opcode
    type: u2
    enum: opcode_enum
  - id: sender_mac_addr
    type: mac_address
  - id: sender_ip_addr
    type: ipv4_address
  - id: target_mac_addr
    type: mac_address
  - id: target_ip_addr
    type: ipv4_address

types:
  mac_address:
    seq:
      - id: addr
        type: u1
        repeat: expr
        repeat-expr: 6

  ipv4_address:
    seq:
      - id: addr
        type: u1
        repeat: expr
        repeat-expr: 4

enums:
  hardware_type_enum:
    1: ethernet
    6: ieee_802
    15: frame_relay
    16: atm
    17: hdlc
    18: fibre_channel

  protocol_type_enum:
    0x0800: ipv4
    0x0806: arp
    0x86dd: ipv6

  opcode_enum:
    1: request
    2: reply
    3: request_reverse
    4: reply_reverse
    5: drarp_request
    6: drarp_reply
    7: drarp_error
    8: inarp_request
    9: inarp_reply