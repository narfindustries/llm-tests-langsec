ICMPType = uint8
ICMPCode = uint8
ICMPChecksum = uint16

ICMPHeader = struct {
  icmpType: ICMPType,
  icmpCode: ICMPCode,
  icmpChecksum: ICMPChecksum
}

ICMPMessage = struct {
  header: ICMPHeader,
  data: bytes
}

ICMPDestinationUnreachable = struct {
  header: ICMPHeader,
  unused: uint32,
  originalIPHeader: bytes,
  additionalData: bytes
}

ICMPSourceQuench = struct {
  header: ICMPHeader,
  unused: uint32,
  originalIPHeader: bytes,
  additionalData: bytes
}

ICMPRedirect = struct {
  header: ICMPHeader,
  gatewayInternetAddress: uint32,
  originalIPHeader: bytes,
  additionalData: bytes
}

ICMPEcho = struct {
  header: ICMPHeader,
  identifier: uint16,
  sequenceNumber: uint16,
  data: bytes
}

ICMPTimeExceeded = struct {
  header: ICMPHeader,
  unused: uint32,
  originalIPHeader: bytes,
  additionalData: bytes
}

ICMPParameterProblem = struct {
  header: ICMPHeader,
  pointer: uint8,
  unused: uint24,
  originalIPHeader: bytes,
  additionalData: bytes
}

ICMPTimestamp = struct {
  header: ICMPHeader,
  identifier: uint16,
  sequenceNumber: uint16,
  originateTimestamp: uint32,
  receiveTimestamp: uint32,
  transmitTimestamp: uint32
}

ICMPAddressMask = struct {
  header: ICMPHeader,
  identifier: uint16,
  sequenceNumber: uint16,
  addressMask: uint32
}

ICMP = struct {
  header: ICMPHeader,
  message: switch(header.icmpType) {
    case 0 => ICMPEcho(header, uint16, uint16, bytes), // Echo Reply
    case 3 => ICMPDestinationUnreachable(header, uint32, bytes, bytes), // Destination Unreachable
    case 4 => ICMPSourceQuench(header, uint32, bytes, bytes), // Source Quench
    case 5 => ICMPRedirect(header, uint32, bytes, bytes), // Redirect
    case 8 => ICMPEcho(header, uint16, uint16, bytes), // Echo Request
    case 11 => ICMPTimeExceeded(header, uint32, bytes, bytes), // Time Exceeded
    case 12 => ICMPParameterProblem(header, uint8, uint24, bytes, bytes), // Parameter Problem
    case 13 => ICMPTimestamp(header, uint16, uint16, uint32, uint32, uint32), // Timestamp Request
    case 14 => ICMPTimestamp(header, uint16, uint16, uint32, uint32, uint32), // Timestamp Reply
    case 17 => ICMPAddressMask(header, uint16, uint16, uint32), // Address Mask Request
    case 18 => ICMPAddressMask(header, uint16, uint16, uint32) // Address Mask Reply
  }
}