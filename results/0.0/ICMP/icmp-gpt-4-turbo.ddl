ICMP : type = struct {
  type : uint8;
  code : uint8;
  checksum : uint16;
  restOfHeader : bytes &length=4;
}

ICMPMessage : type = switch (ICMP.type) {
  0 => EchoReply : ICMP_Echo;
  3 => DestinationUnreachable : ICMP_DestinationUnreachable;
  4 => SourceQuench : ICMP;
  5 => Redirect : ICMP_Redirect;
  8 => Echo : ICMP_Echo;
  11 => TimeExceeded : ICMP;
  12 => ParameterProblem : ICMP_ParameterProblem;
  13 => Timestamp : ICMP_Timestamp;
  14 => TimestampReply : ICMP_Timestamp;
  15 => InformationRequest : ICMP_Information;
  16 => InformationReply : ICMP_Information;
}

ICMP_Echo : type = struct {
  identifier : uint16;
  sequenceNumber : uint16;
  data : bytes;
}

ICMP_DestinationUnreachable : type = struct {
  unused : uint16 &length=2;
  nextHopMTU : uint16;
  data : bytes;
}

ICMP_Redirect : type = struct {
  gatewayInternetAddress : uint32;
  data : bytes;
}

ICMP_ParameterProblem : type = struct {
  pointer : uint8;
  unused : uint8 &length=3;
  data : bytes;
}

ICMP_Timestamp : type = struct {
  identifier : uint16;
  sequenceNumber : uint16;
  originateTimestamp : uint32;
  receiveTimestamp : uint32;
  transmitTimestamp : uint32;
}

ICMP_Information : type = struct {
  identifier : uint16;
  sequenceNumber : uint16;
  data : bytes;
}