ICMPMessage ::= struct {
  type : uint8;
  code : uint8;
  checksum : uint16;
  restOfHeader : uint32;

  data : switch (type) {
    case 0 => EchoReply;
    case 3 => DestinationUnreachable;
    case 4 => SourceQuench;
    case 5 => RedirectMessage;
    case 8 => EchoRequest;
    case 11 => TimeExceeded;
    case 12 => ParameterProblem;
    case 13 => TimestampMessage;
    case 14 => TimestampReply;
    case 15 => InformationRequestReply;
    case 16 => InformationRequestReply;
    case 17 => AddressMaskMessage;
    case 18 => AddressMaskMessage;
  };
};

EchoReply ::= struct {
  identifier : uint16;
  sequenceNumber : uint16;
  data : bytes;
};

DestinationUnreachable ::= struct {
  unused : uint16;
  nextHopMtu : uint16;
  ipHeader : bytes[20];
  first8BytesOfOriginalDatagram : bytes[8];
};

SourceQuench ::= struct {
  unused : uint32;
  ipHeader : bytes[20];
  first8BytesOfOriginalDatagram : bytes[8];
};

RedirectMessage ::= struct {
  gatewayInternetAddress : uint32;
  ipHeader : bytes[20];
  first8BytesOfOriginalDatagram : bytes[8];
};

EchoRequest ::= struct {
  identifier : uint16;
  sequenceNumber : uint16;
  data : bytes;
};

TimeExceeded ::= struct {
  unused : uint16;
  nextHopMtu : uint16;
  ipHeader : bytes[20];
  first8BytesOfOriginalDatagram : bytes[8];
};

ParameterProblem ::= struct {
  pointer : uint8;
  unused : bytes[3];
  ipHeader : bytes[20];
  first8BytesOfOriginalDatagram : bytes[8];
};

TimestampMessage ::= struct {
  identifier : uint16;
  sequenceNumber : uint16;
  originateTimestamp : uint32;
  receiveTimestamp : uint32;
  transmitTimestamp : uint32;
};

TimestampReply ::= struct {
  identifier : uint16;
  sequenceNumber : uint16;
  originateTimestamp : uint32;
  receiveTimestamp : uint32;
  transmitTimestamp : uint32;
};

InformationRequestReply ::= struct {
  identifier : uint16;
  sequenceNumber : uint16;
};

AddressMaskMessage ::= struct {
  identifier : uint16;
  sequenceNumber : uint16;
  addressMask : uint32;
};