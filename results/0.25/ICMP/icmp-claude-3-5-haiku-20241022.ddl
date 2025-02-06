protocol ICMPMessage {
  type: uint8 = {
    0: EchoReply,
    3: DestinationUnreachable,
    4: SourceQuench,
    5: Redirect,
    8: EchoRequest,
    9: RouterAdvertisement,
    10: RouterSolicitation,
    11: TimeExceeded,
    12: ParameterProblem,
    13: Timestamp,
    14: TimestampReply,
    15: InformationRequest,
    16: InformationReply
  };

  code: uint8 = match type {
    DestinationUnreachable => {
      0: NetUnreachable,
      1: HostUnreachable,
      2: ProtocolUnreachable,
      3: PortUnreachable,
      4: FragmentationNeeded,
      5: SourceRouteFailed,
      6: NetworkUnknown,
      7: HostUnknown
    }
  };

  checksum: uint16;

  payload: match type {
    EchoReply | EchoRequest => {
      identifier: uint16,
      sequenceNumber: uint16,
      data: bytes
    },
    Timestamp | TimestampReply => {
      identifier: uint16,
      sequenceNumber: uint16,
      originateTimestamp: uint32,
      receiveTimestamp: uint32,
      transmitTimestamp: uint32
    },
    Redirect => {
      gatewayAddress: ipv4,
      originalDatagramHeader: bytes
    },
    TimeExceeded | ParameterProblem => {
      pointer: uint8,
      originalDatagramHeader: bytes
    },
    RouterAdvertisement => {
      numAddresses: uint8,
      addressEntries: list(RouterAddressEntry, numAddresses)
    }
  };
}

struct RouterAddressEntry {
  routerAddress: ipv4,
  preferenceLevel: uint32
}