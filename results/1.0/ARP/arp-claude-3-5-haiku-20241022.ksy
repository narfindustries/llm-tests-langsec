meta:
  id: arp
  title: Address Resolution Protocol
  endian: be
seq:
  - id: hardware_type
    type: u2
    enum: hardware_type_enum
  - id: protocol_type
    type: u2
    enum: protocol_type_enum
  - id: hardware_addr_length
    type: u1
  - id: protocol_addr_length
    type: u1
  - id: operation
    type: u2
    enum: operation_enum
  - id: sender_hardware_addr
    type: str
    size: hardware_addr_length
    encoding: ascii
  - id: sender_protocol_addr
    type: str
    size: protocol_addr_length
    encoding: ascii
  - id: target_hardware_addr
    type: str
    size: hardware_addr_length
    encoding: ascii
  - id: target_protocol_addr
    type: str
    size: protocol_addr_length
    encoding: ascii
enums:
  hardware_type_enum:
    1: ethernet
    6: ieee_802
    15: framerelay
  protocol_type_enum:
    0x0800: ipv4
    0x0806: arp
    0x86DD: ipv6
  operation_enum:
    1: request
    2: reply