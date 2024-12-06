meta:
  id: ntp_v4
  endian: be

seq:
  - id: flags
    type: u1
  - id: stratum
    type: u1
  - id: poll
    type: s1
  - id: precision
    type: s1
  - id: root_delay
    type: s4
  - id: root_dispersion
    type: s4
  - id: reference_id
    type: u4
  - id: reference_timestamp
    type: ntp_timestamp
  - id: origin_timestamp
    type: ntp_timestamp
  - id: receive_timestamp
    type: ntp_timestamp
  - id: transmit_timestamp
    type: ntp_timestamp
  - id: extension_fields
    type: extension
    repeat: until
    repeat-until: _.next_field == extension_type::eop or _io.eof

types:
  ntp_timestamp:
    seq:
      - id: seconds
        type: u4
      - id: fraction
        type: u4

  extension:
    seq:
      - id: next_field
        type: u2
        enum: extension_type
      - id: length
        type: u2
      - id: value
        size: length - 4
        type:
          switch-on: next_field
          cases:
            'extension_type::mac': mac_data

  mac_data:
    seq:
      - id: key_id
        type: u4
      - id: mac_length
        type: u2
      - id: mac
        size: mac_length

enums:
  extension_type:
    0: eop
    1: mac