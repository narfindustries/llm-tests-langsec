meta:
  id: arp
  title: Address Resolution Protocol (ARP) Packet
  endian: be
seq:
  - id: hardware_type
    type: u2
    doc: Hardware address type (e.g., Ethernet = 1)
  - id: protocol_type
    type: u2
    doc: Protocol address type (e.g., IPv4 = 0x0800)
  - id: hardware_addr_length
    type: u1
    doc: Length of hardware address in bytes
  - id: protocol_addr_length
    type: u1
    doc: Length of protocol address in bytes
  - id: operation
    type: u2
    enum: arp_opcode
    doc: ARP operation (request or reply)
  - id: sender_hardware_addr
    type: bytes
    size: hardware_addr_length
    doc: Sender's hardware (MAC) address
  - id: sender_protocol_addr
    type: bytes
    size: protocol_addr_length
    doc: Sender's protocol (IP) address
  - id: target_hardware_addr
    type: bytes
    size: hardware_addr_length
    doc: Target's hardware (MAC) address
  - id: target_protocol_addr
    type: bytes
    size: protocol_addr_length
    doc: Target's protocol (IP) address

enums:
  arp_opcode:
    1: request
    2: reply