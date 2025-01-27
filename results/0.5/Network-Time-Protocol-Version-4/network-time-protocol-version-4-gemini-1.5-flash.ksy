meta:
  id: network-time-protocol-version-4-gemini-1
  title: Network Time Protocol Version 4 (Gemini 1)
  license: Apache-2.0
  homepage: https://github.com/kaitai-io/kaitai_struct_formats

types:
  ntp_header:
    seq:
      - id: leap
        type: u1
      - id: version
        type: u1
      - id: mode
        type: u1
      - id: stratum
        type: u1
      - id: poll
        type: u1
      - id: precision
        type: u1
      - id: root_delay
        type: u4be
      - id: root_dispersion
        type: u4be
      - id: ref_id
        type: u4be
      - id: ref_timestamp
        type: u8be
      - id: orig_timestamp
        type: u8be
      - id: recv_timestamp
        type: u8be
      - id: xmit_timestamp
        type: u8be

  ntp_packet:
    seq:
      - id: header
        type: ntp_header


