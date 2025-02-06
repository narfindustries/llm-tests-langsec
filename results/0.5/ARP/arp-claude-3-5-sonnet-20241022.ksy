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
    enum: hardware_types
  - id: protocol_type
    type: u2
    enum: protocol_types
  - id: hardware_addr_len
    type: u1
  - id: protocol_addr_len
    type: u1
  - id: operation
    type: u2
    enum: operations
  - id: sender_hardware_addr
    size: hardware_addr_len
  - id: sender_protocol_addr
    size: protocol_addr_len
  - id: target_hardware_addr
    size: hardware_addr_len
  - id: target_protocol_addr
    size: protocol_addr_len

enums:
  hardware_types:
    1: ethernet_10mb
    6: ieee_802
    7: arcnet
  protocol_types:
    0x0800: ipv4
  operations:
    1: request
    2: reply
    3: rarp_request
    4: rarp_reply