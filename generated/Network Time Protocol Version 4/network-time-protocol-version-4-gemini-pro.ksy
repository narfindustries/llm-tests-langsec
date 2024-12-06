id: network-time-protocol-version-4
meta:
  id: network-time-protocol-version-4
  name: Network Time Protocol Version 4
  description: Network Time Protocol (NTP) is a networking protocol for clock synchronization between computer systems over packet-switched, variable-latency data networks. In operation since 1985, NTP is one of the oldest Internet protocols in current use. NTP was designed by David L. Mills of the University of Delaware.
  version: 1.0.0
params:
- name: Leap Indicator
  size: 1
  description: The leap indicator is a 2-bit field that is used to indicate the status of the NTP server.
  id: leap_indicator
  enums:
  - id: no_warning
    name: No Warning
    value: 0
  - id: last_minute_of_day
    name: Last Minute of Day
    value: 1
  - id: last_minute_of_week
    name: Last Minute of Week
    value: 2
  - id: alarm_condition
    name: Alarm Condition
    value: 3
  type: u_int8_t
- name: Version Number
  size: 1
  description: The version number is a 3-bit field that indicates the version of the NTP protocol that is being used.
  id: version_number
  enums:
  - id: version_1
    name: Version 1
    value: 1
  - id: version_2
    name: Version 2
    value: 2
  - id: version_3
    name: Version 3
    value: 3
  - id: version_4
    name: Version 4
    value: 4
  type: u_int8_t
- name: Mode
  size: 1
  description: The mode field is a 3-bit field that indicates the mode of the NTP server.
  id: mode
  enums:
  - id: symmetric_active
    name: Symmetric Active
    value: 0
  - id: symmetric_passive
    name: Symmetric Passive
    value: 1
  - id: client
    name: Client
    value: 2
  - id: server
    name: Server
    value: 3
  - id: broadcast
    name: Broadcast
    value: 4
  - id: control_message
    name: Control Message
    value: 5
  - id: private
    name: Private
    value: 6
  - id: reserved
    name: Reserved
    value: 7
  type: u_int8_t
- name: Stratum
  size: 1
  description: The stratum field is a 8-bit field that indicates the stratum of the NTP server.
  id: stratum
  type: u_int8_t
- name: Poll Interval
  size: 1
  description: The poll interval is a 8-bit field that indicates the poll interval of the NTP server.
  id: poll_interval
  type: u_int8_t
- name: Precision
  size: 1
  description: The precision field is a 8-bit field that indicates the precision of the NTP server.
  id: precision
  type: u_int8_t
- name: Root Delay
  size: 4
  description: The root delay is a 32-bit field that indicates the root delay of the NTP server.
  id: root_delay
  type: int32_t
- name: Root Dispersion
  size: 4
  description: The root dispersion is a 32-bit field that indicates the root dispersion of the NTP server.
  id: root_dispersion
  type: int32_t
- name: Reference Identifier
  size: 4
  description: The reference identifier is a 4-character field that indicates the reference identifier of the NTP server.
  id: reference_identifier
  type: string
- name: Reference Timestamp
  size: 8
  description: The reference timestamp is a 64-bit field that indicates the reference timestamp of the NTP server.
  id: reference_timestamp
  type: timestamp
- name: Originate Timestamp
  size: 8
  description: The originate timestamp is a 64-bit field that indicates the originate timestamp of the NTP server.
  id: originate_timestamp
  type: timestamp
- name: Receive Timestamp
  size: 8
  description: The receive timestamp is a 64-bit field that indicates the receive timestamp of the NTP server.
  id: receive_timestamp
  type: timestamp
- name: Transmit Timestamp
  size: 8
  description: The transmit timestamp is a 64-bit field that indicates the transmit timestamp of the NTP server.
  id: transmit_timestamp
  type: timestamp