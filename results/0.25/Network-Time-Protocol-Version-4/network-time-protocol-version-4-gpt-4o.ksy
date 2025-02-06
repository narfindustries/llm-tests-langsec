meta:
  id: ntp_packet
  title: Network Time Protocol Version 4
  application: ntp
  file-extension: ntp
  endian: be
seq:
  - id: li_vn_mode
    type: u1
  - id: stratum
    type: u1
  - id: poll
    type: u1
  - id: precision
    type: u1
  - id: root_delay
    type: u4
  - id: root_dispersion
    type: u4
  - id: reference_identifier
    type: u4
  - id: reference_timestamp
    type: u8
  - id: originate_timestamp
    type: u8
  - id: receive_timestamp
    type: u8
  - id: transmit_timestamp
    type: u8
instances:
  leap_indicator:
    value: (li_vn_mode >> 6) & 0x03
  version_number:
    value: (li_vn_mode >> 3) & 0x07
  mode:
    value: li_vn_mode & 0x07