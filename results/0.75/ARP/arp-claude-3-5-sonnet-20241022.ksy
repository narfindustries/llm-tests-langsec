meta:
  id: arp
  title: Address Resolution Protocol
  file-extension: arp
  xref:
    rfc: 826
  endian: be
  license: CC0-1.0
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
  - id: sender_hardware_address
    size: hardware_size
  - id: sender_protocol_address
    size: protocol_size
  - id: target_hardware_address
    size: hardware_size
  - id: target_protocol_address
    size: protocol_size
enums:
  hardware_type_enum:
    1: ethernet_10mb
  protocol_type_enum:
    0x0800: ipv4
  opcode_enum:
    1: request
    2: reply