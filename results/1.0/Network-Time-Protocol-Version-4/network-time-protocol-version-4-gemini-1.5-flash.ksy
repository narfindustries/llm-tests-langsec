meta:
  id: ntp_v4
types:
  ntp_timestamp:
    seq:
      - id: int_part
        type: u4be
      - id: fraction
        type: u4be
  ntp_packet:
    seq:
      - id: li
        type: u2be
      - id: vn
        type: u3be
      - id: mode
        type: u3be
      - id: stratum
        type: u1
      - id: poll
        type: u1
      - id: precision
        type: u1
      - id: root_delay
        type: f8be
      - id: root_dispersion
        type: f8be
      - id: ref_id
        type: u4be
      - id: ref_timestamp
        type: ntp_timestamp
      - id: orig_timestamp
        type: ntp_timestamp
      - id: recv_timestamp
        type: ntp_timestamp
      - id: xmit_timestamp
        type: ntp_timestamp
      - id: key_id
        type: u4be
        if: (mode & 0x08) != 0
      - id: digest_len
        type: u4be
        if: (mode & 0x08) != 0
      - id: digest
        type: bytes
        if: (mode & 0x08) != 0
        size: digest_len

