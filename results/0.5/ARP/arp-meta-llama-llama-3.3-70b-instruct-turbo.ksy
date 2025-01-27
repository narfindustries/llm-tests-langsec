meta:
  id: arp
  title: Address Resolution Protocol
  file-format: network-protocol
seq:
  - id: hardware_type
    type: u2
  - id: protocol_type
    type: u2
  - id: hardware_address_length
    type: u1
  - id: protocol_address_length
    type: u1
  - id: operation
    type: u2
  - id: sender_hardware_address
    type: bytes
    size: hardware_address_length
  - id: sender_protocol_address
    type: bytes
    size: protocol_address_length
  - id: target_hardware_address
    type: bytes
    size: hardware_address_length
  - id: target_protocol_address
    type: bytes
    size: protocol_address_length