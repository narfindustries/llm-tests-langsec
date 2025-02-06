meta:
  id: arp
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
  - id: oper
    type: u2
  - id: sha
    type: bytes
    size: hlen
  - id: spa
    type: bytes
    size: plen
  - id: tha
    type: bytes
    size: hlen
  - id: tpa
    type: bytes
    size: plen
enums:
  htype_enum:
    1: ethernet
    2: experimental_ethernet
    3: amateur_radio_ax_25
    4: proteon_pronet_token_ring
    5: chaos
    6: ieee_802_networks
    7: arcnet
    8: hyperchannel
    9: lanstar
    10: autonet_short_address
    11: localtalk
    12: localnet
    13: ultra_link
    14: smds
    15: frame_relay
    16: atm
    17: hdlc
    18: fibre_channel
    19: atm_other
    20: serial_line
    21: atm_p2p
    22: mil_std_188_220
    23: metricom
    24: ieee_1394_1995
    25: mapos
    26: twinaxial
    27: eui_64
    28: hiparp
    29: ip_and_arp_over_iso_7816_1
    30: arp_in_arpanet
    31: ip_and_arp_over_fibre_channel
    32: hiparp_ip
  ptype_enum:
    2048: ipv4
    2054: arp
    32821: rarp
    9053: trill
    24579: decnet_phase_iv
  oper_enum:
    1: request
    2: reply
    3: request_reverse
    4: reply_reverse