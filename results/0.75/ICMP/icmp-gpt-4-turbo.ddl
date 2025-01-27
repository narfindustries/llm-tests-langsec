module ICMP {
  import Network.IPv4

  type IcmpType = {
    typeCode : UInt8,
    code : UInt8,
    checksum : UInt16,
    restOfHeader : UInt32
  }

  type IPv4Packet = {
    versionIHL : UInt8,
    typeOfService : UInt8,
    totalLength : UInt16,
    identification : UInt16,
    flagsFragmentOffset : UInt16,
    ttl : UInt8,
    protocol : UInt8,
    headerChecksum : UInt16,
    srcAddr : IPv4,
    dstAddr : IPv4,
    optionsPadding : [UInt8]  // Variable length based on IHL
  }

  type IcmpEchoRequestOrReply = {
    header : IcmpType,
    identifier : UInt16,
    sequenceNumber : UInt16,
    data : [UInt8]
  }

  type IcmpMessage = union (UInt8) {
    case 0 | 8 => Echo : IcmpEchoRequestOrReply,
    case _ => Unknown : {}
  }

  type IcmpPacket = {
    ipHeader : IPv4Packet,
    icmpMessage : IcmpMessage
  }

  let icmpPacket = parse IcmpPacket
}