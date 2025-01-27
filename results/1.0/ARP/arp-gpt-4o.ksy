meta:
  id: arp_packet
  title: ARP Packet
  file-extension: arp
  endian: be

seq:
  - id: htype
    type: u2
  - id: ptype
    type: u2
  - id: hlen
    type: u1
  - id: plen
    type: u1
  - id: operation
    type: u2
    enum: arp_operation
  - id: sender_hw_addr
    type: hw_address
    size: hlen
  - id: sender_proto_addr
    type: bignum
    size: plen
  - id: target_hw_addr
    type: hw_address
    size: hlen
  - id: target_proto_addr
    type: bignum
    size: plen

types:
  hw_address:
    seq:
      - id: addr
        type: u1
        repeat: expr
        repeat-expr: _parent.hlen

  bignum:
    seq:
      - id: number
        type: u1
        repeat: expr
        repeat-expr: _parent.plen

enums:
  arp_operation:
    1: request
    2: reply
