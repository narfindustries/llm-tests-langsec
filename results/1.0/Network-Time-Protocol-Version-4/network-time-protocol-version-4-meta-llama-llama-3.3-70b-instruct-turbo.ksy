meta:
  id: ntp_packet
  endian: be
seq:
  - id: leap
    type: u2
  - id: version
    type: u3
  - id: mode
    type: u3
  - id: poll
    type: u8
  - id: precision
    type: s8
  - id: root_delay
    type: u4
  - id: root_dispersion
    type: u4
  - id: ref_id
    type:
      switch-on: version
      cases:
        '3': ref_id_ipv4
        '4': ref_id_ascii
      ref_id_ipv4:
        type: ip_addr
      ref_id_ascii:
        type: str
        size: 4
  - id: ref_timestamp
    type: ntp_timestamp
  - id: orig_timestamp
    type: ntp_timestamp
  - id: recv_timestamp
    type: ntp_timestamp
  - id: xmt_timestamp
    type: ntp_timestamp
  - id: extension_fields
    type: extension_field
    repeat: expr
    until: (_.mode == 6)
types:
  ntp_timestamp:
    seq:
      - id: seconds
        type: u4
      - id: fraction
        type: u4
  ip_addr:
    seq:
      - id: octet1
        type: u1
      - id: octet2
        type: u1
      - id: octet3
        type: u1
      - id: octet4
        type: u1
  extension_field:
    seq:
      - id: type
        type: u2
        enum: extension_types
      - id: length
        type: u2
      - id: value
        type: str
        size: length
        encoding: ascii
      - id: padding
        type: padding
        size: (4 - (length % 4)) % 4
  extension_types:
    0: end_of_message
    1: server
    2: peer
    3: client
    4: active
    5: passive
    6: broadcast
    7: reserved
  padding:
    seq:
      - id: padding_bytes
        type: u1
        repeat: expr