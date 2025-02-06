seq NTPv4Packet {
  leapIndicator: uint2;
  versionNumber: uint3;
  mode: uint3;
  poll: uint8;
  precision: int8;
  rootDelay: uint32;
  rootDispersion: uint32;
  refId: uint32;
  refTimestamp: uint64;
  origTimestamp: uint64;
  recvTimestamp: uint64;
  xmitTimestamp: uint64;
  extensions: seq ExtensionField;
}

seq ExtensionField {
  fieldType: uint16;
  length: uint16;
  value: bytes length;
}

seq KissODeath {
  code: uint8;
}

seq ReferenceClockIdentifier {
  type: uint8;
  identifier: bytes 3;
}

seq Auth {
  type: uint8;
  data: bytes 20;
}