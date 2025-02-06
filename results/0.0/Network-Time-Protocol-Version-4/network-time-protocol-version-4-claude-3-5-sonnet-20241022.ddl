def Main = {
  li uint2 where {
    0 -> "no warning"
    1 -> "last minute has 61 seconds"
    2 -> "last minute has 59 seconds"
    3 -> "alarm condition"
  }
  vn uint3 where {
    4 -> "NTP v4"
  }
  mode uint3 where {
    0 -> "reserved"
    1 -> "symmetric active"
    2 -> "symmetric passive"
    3 -> "client"
    4 -> "server"
    5 -> "broadcast"
    6 -> "NTP control message"
    7 -> "reserved private"
  }
  stratum uint8 where {
    0 -> "unspecified"
    1 -> "primary reference"
    2..15 -> "secondary reference"
    16 -> "unsynchronized"
    17..255 -> "reserved"
  }
  poll int8
  precision int8
  rootDelay int32
  rootDispersion uint32
  referenceID bytes(4)
  referenceTimestamp {
    seconds uint32
    fraction uint32
  }
  originTimestamp {
    seconds uint32
    fraction uint32
  }
  receiveTimestamp {
    seconds uint32
    fraction uint32
  }
  transmitTimestamp {
    seconds uint32
    fraction uint32
  }
  extensions [{
    fieldType uint16
    length uint16
    value bytes(length - 4)
    padding bytes((4 - (length % 4)) % 4)
  }]
  mac ?{
    keyID uint32
    digest bytes(20)
  }
}