meta:
  id: ntp_v4
  title: Network Time Protocol Version 4
  endian: be
  xref:
    rfc: 5905
seq:
  - id: li_vn_mode
    type: u1
  - id: stratum
    type: u1
  - id: poll
    type: u1
  - id: precision
    type: s1
  - id: root_delay
    type: u4
  - id: root_dispersion
    type: u4
  - id: reference_id
    type: u4
  - id: reference_timestamp
    type: u8
  - id: originate_timestamp
    type: u8
  - id: receive_timestamp
    type: u8
  - id: transmit_timestamp
    type: u8
  - id: extension_fields
    type: extension_field
    repeat: eos
types:
  extension_field:
    seq:
      - id: field_type
        type: u2
      - id: length
        type: u2
      - id: field_value
        size: length
      - id: padding
        type: u1
        repeat: expr
        repeat-expr: ((4 - length % 4) % 4)
instances:
  li:
    value: '((li_vn_mode & 0xC0) >> 6)'
  vn:
    value: '((li_vn_mode & 0x38) >> 3)'
  mode:
    value: '(li_vn_mode & 0x07)'