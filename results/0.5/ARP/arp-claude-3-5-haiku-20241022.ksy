meta:
  id: arp
  title: Address Resolution Protocol
  endian: be

seq:
  - id: hardware_type
    type: u2
    enum: hardware_types
  - id: protocol_type
    type: u2
    enum: protocol_types
  - id: hardware_address_length
    type: u1
  - id: protocol_address_length
    type: u1
  - id: operation
    type: u2
    enum: operation_types
  - id: sender_hardware_address
    type: hardware_address
    size: hardware_address_length
  - id: sender_protocol_address
    type: protocol_address
    size: protocol_address_length
  - id: target_hardware_address
    type: hardware_address
    size: hardware_address_length
  - id: target_protocol_address
    type: protocol_address
    size: protocol_address_length

types:
  hardware_address:
    seq:
      - id: address
        type: u1
        repeat: expr
        repeat-expr: _parent.hardware_address_length

  protocol_address:
    seq:
      - id: address
        type: u1
        repeat: expr
        repeat-expr: _parent.protocol_address_length

enums:
  hardware_types:
    1: ethernet
    2: experimental_ethernet
    3: amateur_radio_ax25
    4: proteon_pronet_token_ring
    5: chaos
    6: ieee_802
    7: arcnet
    8: hyperchannel
    9: lanstar
    10: autonet_short_address
    11: localtalk
    12: localnet
    13: ultra_link
    14: smds
    15: frame_relay
    16: asynchronous_transmission_mode
    17: hdlc
    18: fibre_channel
    19: asynchronous_transmission_mode_fiber
    20: serial_line

  protocol_types:
    0x0800: ipv4
    0x0806: arp
    0x86dd: ipv6
    0x8100: vlan
    0x88a8: provider_bridging
    0x8847: mpls_unicast
    0x8848: mpls_multicast

  operation_types:
    1: request
    2: reply
    3: reverse_request
    4: reverse_reply
    5: drarp_request
    6: drarp_reply
    7: drarp_error
    8: inarp_request
    9: inarp_reply