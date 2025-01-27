meta:
  id: arp_packet
  title: ARP Packet
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
  - id: sender_hw_addr
    type: hw_address
    size: hlen
  - id: sender_proto_addr
    type: proto_address
    size: plen
  - id: target_hw_addr
    type: hw_address
    size: hlen
  - id: target_proto_addr
    type: proto_address
    size: plen

types:
  hw_address:
    seq:
      - id: addr
        type: u1
        repeat: expr
        repeat-expr: _parent.hlen

  proto_address:
    seq:
      - id: addr
        type: u1
        repeat: expr
        repeat-expr: _parent.plen