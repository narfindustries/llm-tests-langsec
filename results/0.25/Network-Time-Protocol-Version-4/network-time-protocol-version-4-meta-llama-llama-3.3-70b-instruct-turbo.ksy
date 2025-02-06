meta:
  id: ntp_packet
  title: Network Time Protocol Version 4
  endian: be
seq:
  - id: leap_indicator
    type: u2
  - id: version_number
    type: u3
  - id: mode
    type: u3
  - id: poll
    type: u8
  - id: precision
    type: u8
  - id: root_delay
    type: u4
  - id: root_dispersion
    type: u4
  - id: ref_id
    type: u4
  - id: ref_timestamp
    type: u8
  - id: origin_timestamp
    type: u8
  - id: receive_timestamp
    type: u8
  - id: transmit_timestamp
    type: u8
  - id: extension_fields
    type: extension_field
    repeat: expr
    repeat_expr: extension_field.length > 0
types:
  extension_field:
    seq:
      - id: type
        type: u2
      - id: length
        type: u2
      - id: value
        type: switch_on
        cases:
          - case: type == 0
            type: reserved
          - case: type == 1
            type: mac
          - case: type == 2
            type: crypto_nak
          - case: type == 3
            type: autokey
          - case: type == 4
            type: nts
          - case: type >= 5
            type: reserved
    types:
      reserved:
        seq:
          - id: reserved_data
            size: 4
            type: u1
      mac:
        seq:
          - id: mac_data
            size: 20
            type: u1
      crypto_nak:
        seq:
          - id: crypto_nak_data
            size: 20
            type: u1
      autokey:
        seq:
          - id: autokey_data
            size: 20
            type: u1
      nts:
        seq:
          - id: nts_data
            size: 20
            type: u1