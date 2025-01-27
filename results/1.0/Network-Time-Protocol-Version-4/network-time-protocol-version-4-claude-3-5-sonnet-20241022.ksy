meta:
  id: ntp_v4_packet
  title: Network Time Protocol Version 4
  file-extension: bin
  endian: be
  license: MIT

doc: |
  NTP version 4 packet format specification based on RFC 5905.
  Used for time synchronization between computer systems.

seq:
  - id: header
    type: header_fields
  - id: origin_timestamp
    type: timestamp
  - id: receive_timestamp
    type: timestamp
  - id: transmit_timestamp
    type: timestamp
  - id: extension_fields
    type: extension_field
    if: header.extensions_present
    repeat: until
    repeat-until: _.end_of_extensions
    
types:
  header_fields:
    seq:
      - id: leap_indicator
        type: b2
        doc: Warning of an impending leap second
      - id: version_number
        type: b3
        doc: NTP version number (4)
      - id: mode
        type: b3
        doc: Protocol mode
      - id: stratum
        type: u1
        doc: Stratum level of the time source
      - id: poll
        type: s1
        doc: Maximum interval between messages
      - id: precision
        type: s1
        doc: Precision of the system clock
      - id: root_delay
        type: u4
        doc: Total round-trip delay to reference clock
      - id: root_dispersion
        type: u4
        doc: Total dispersion to reference clock
      - id: reference_id
        type: u4
        doc: Reference clock identifier
      - id: reference_timestamp
        type: timestamp
        doc: Time when the system clock was last set or corrected
    instances:
      extensions_present:
        value: mode == 7
        doc: True if packet contains extension fields

  timestamp:
    seq:
      - id: seconds
        type: u4
        doc: Seconds since January 1, 1900
      - id: fraction
        type: u4
        doc: Fraction of a second

  extension_field:
    seq:
      - id: type
        type: u2
        doc: Type of extension field
      - id: length
        type: u2
        doc: Length of extension field including header
      - id: value
        size: length - 4
        if: length > 4
        doc: Extension field value
    instances:
      end_of_extensions:
        value: type == 0
        doc: True if this is the last extension field

enums:
  mode:
    0: reserved
    1: symmetric_active
    2: symmetric_passive 
    3: client
    4: server
    5: broadcast
    6: control_message
    7: private

  leap:
    0: no_warning
    1: last_minute_61
    2: last_minute_59
    3: alarm