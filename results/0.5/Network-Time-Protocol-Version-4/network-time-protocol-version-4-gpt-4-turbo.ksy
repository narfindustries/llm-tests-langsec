meta:
  id: ntp_packet
  title: Network Time Protocol Version 4
  endian: be
  xref:
    rfc: 5905
seq:
  - id: leap_indicator
    type: b2
    enum: leap_indicator
  - id: version
    type: b3
  - id: mode
    type: b3
    enum: mode
  - id: stratum
    type: u1
  - id: poll
    type: u1
  - id: precision
    type: s1
  - id: root_delay
    type: ntp_short
  - id: root_dispersion
    type: ntp_short
  - id: reference_id
    type: u4
  - id: reference_timestamp
    type: ntp_timestamp
  - id: originate_timestamp
    type: ntp_timestamp
  - id: receive_timestamp
    type: ntp_timestamp
  - id: transmit_timestamp
    type: ntp_timestamp
types:
  ntp_short:
    seq:
      - id: seconds
        type: s2
      - id: fraction
        type: u2
  ntp_timestamp:
    seq:
      - id: seconds
        type: u4
      - id: fraction
        type: u4
enums:
  leap_indicator:
    0: no_warning
    1: last_minute_61_seconds
    2: last_minute_59_seconds
    3: alarm_condition
  mode:
    0: reserved
    1: symmetric_active
    2: symmetric_passive
    3: client
    4: server
    5: broadcast
    6: control_message
    7: private_use