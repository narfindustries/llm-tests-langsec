type LeapIndicator = enum { NoWarning = 0, LastMinuteHas61Seconds = 1, LastMinuteHas59Seconds = 2, AlarmCondition = 3 }
type VersionNumber = enum { NtpVersion4 = 4 }
type Mode = enum { Reserved = 0, SymmetricActive = 1, SymmetricPassive = 2, Client = 3, Server = 4, Broadcast = 5, ReservedForNtpControlMessage = 6, ReservedForPrivateUse = 7 }
type ReferenceClockIdentifier = uint32

type NtpHeader = struct {
  leapIndicator: uint2,
  versionNumber: uint3,
  mode: uint3,
  poll: uint8,
  precision: int8,
  rootDelay: uint32,
  rootDispersion: uint32,
  referenceClockIdentifier: ReferenceClockIdentifier,
  referenceTimestamp: uint64,
  originTimestamp: uint64,
  receiveTimestamp: uint64,
  transmitTimestamp: uint64,
}

type Mac = struct {
  type: uint16,
  length: uint16,
  authenticationData: bytes,
}

type UniqueIdentifier = struct {
  type: uint16,
  length: uint16,
  uniqueIdentifier: bytes,
}

type ExtensionData = struct {
  type: uint16,
  length: uint16,
  extensionData: bytes,
}

type NtpExtension = choice {
  mac: Mac,
  uniqueIdentifier: UniqueIdentifier,
  extensionData: ExtensionData,
}

type NtpPacket = struct {
  header: NtpHeader,
  extensions: array(NtpExtension),
}