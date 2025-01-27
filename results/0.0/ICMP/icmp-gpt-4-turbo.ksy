meta:
  id: icmp_packet
  title: ICMP (Internet Control Message Protocol) Packet
  license: CC0-1.0
  endian: be

doc: |
  The Internet Control Message Protocol (ICMP) is used by network devices,
  like routers, to send error messages and operational information.

seq:
  - id: type
    type: u1
    doc: Type of ICMP packet.
  - id: code
    type: u1
    doc: Subtype to the given message type.
  - id: checksum
    type: u2
    doc: Error-checking data calculated from the ICMP header and data.
  - id: rest_of_header
    type:
      switch-on: type
      cases:
        0: echo
        8: echo
        3: dest_unreachable
        11: time_exceeded
        5: redirect
        13: timestamp
        14: timestamp_reply
        17: address_mask
        18: address_mask_reply
    doc: Rest of the header varies depending on the type.

types:
  echo:
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2
      - id: data
        size-eos: true

  dest_unreachable:
    seq:
      - id: unused
        type: u4
      - id: next_hop_mtu
        type: u2
      - id: data
        size-eos: true

  time_exceeded:
    seq:
      - id: unused
        type: u4
      - id: data
        size-eos: true

  redirect:
    seq:
      - id: gateway_internet_address
        type: u4
      - id: data
        size-eos: true

  timestamp:
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2
      - id: originate_timestamp
        type: u4
      - id: receive_timestamp
        type: u4
      - id: transmit_timestamp
        type: u4

  timestamp_reply:
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2
      - id: originate_timestamp
        type: u4
      - id: receive_timestamp
        type: u4
      - id: transmit_timestamp
        type: u4

  address_mask:
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2
      - id: address_mask
        type: u4

  address_mask_reply:
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2
      - id: address_mask
        type: u4