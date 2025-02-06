type LeapIndicator = bits(2)
type VersionNumber = bits(3)
type Mode = bits(3)
type Stratum = uint8
type Poll = int8
type Precision = int8
type RootDelay = int32
type RootDispersion = uint32
type ReferenceIdentifier = uint32

struct Timestamp {
  seconds : uint32
  fraction : uint32
}

struct NTPPacket {
  li : LeapIndicator
  vn : VersionNumber
  mode : Mode
  stratum : Stratum
  poll : Poll
  precision : Precision
  rootDelay : RootDelay
  rootDispersion : RootDispersion
  refId : ReferenceIdentifier
  refTs : Timestamp
  origTs : Timestamp
  recvTs : Timestamp
  xmitTs : Timestamp
  extFields : array(uint8) // Optional, variable length
  keyId : uint32 // Optional
  digest : array(uint8) // Optional, 16 bytes (128 bits)
}

type NTPMessage = NTPPacket // Root type