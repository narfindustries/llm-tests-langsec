meta:
  id: arp
  title: Address Resolution Protocol
  file-extension: arp
  xref:
    rfc: 826
  license: CC0-1.0
  endian: be

seq:
  - id: hardware_type
    type: u2
    enum: hardware_type_enum
  - id: protocol_type
    type: u2
  - id: hardware_addr_len
    type: u1
  - id: protocol_addr_len
    type: u1
  - id: operation
    type: u2
    enum: operation_enum
  - id: sender_hardware_addr
    size: hardware_addr_len
  - id: sender_protocol_addr
    size: protocol_addr_len
  - id: target_hardware_addr
    size: hardware_addr_len
  - id: target_protocol_addr
    size: protocol_addr_len

enums:
  hardware_type_enum:
    1: ethernet_10mb
    6: ieee_802
    7: arcnet
    15: frame_relay
    16: atm
    18: hdlc
    19: fibre_channel

  operation_enum:
    1: request
    2: reply

instances:
  is_ethernet:
    value: hardware_type == hardware_type_enum::ethernet_10mb
  is_ipv4:
    value: protocol_type == 0x0800