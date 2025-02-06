format uint2 LeapIndicator {
  0 = no_warning,
  1 = last_minute_has_61_seconds,
  2 = last_minute_has_59_seconds,
  3 = alarm_condition
}

format uint3 VersionNumber {
  1 = NTP_version_3,
  2 = NTP_version_2,
  3 = NTP_version_1,
  4 = NTP_version_4
}

format uint3 Mode {
  0 = reserved,
  1 = symmetric_active,
  2 = symmetric_passive,
  3 = client,
  4 = server,
  5 = broadcast,
  6 = control,
  7 = reserved_7
}

format choice RefID {
  0 = unknown,
  1..255 = ReferenceClock : uint8,
  256..65535 = IPv4Address : uint16,
  65536..4294967295 = IPv6Address : uint32
}

format bytes ExtensionFieldData {
  length : uint16,
  data : bytes(length)
}

format struct NTPv4Packet {
  leap_indicator : LeapIndicator,
  version_number : VersionNumber,
  mode : Mode,
  poll : uint8,
  precision : int8,
  root_delay : uint32,
  root_dispersion : uint32,
  ref_id : RefID,
  reference_timestamp : uint64,
  origin_timestamp : uint64,
  receive_timestamp : uint64,
  transmit_timestamp : uint64,
  extension_fields : array {
    while true {
      type : uint16,
      length : uint16,
      data : ExtensionFieldData,
      break when type == 0 and length == 0
    }
  }
}