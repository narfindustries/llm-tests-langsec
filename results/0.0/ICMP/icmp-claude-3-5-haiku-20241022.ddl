type ICMPType =
  | EchoReply
  | DestinationUnreachable
  | SourceQuench
  | Redirect
  | EchoRequest
  | RouterAdvertisement
  | RouterSolicitation
  | TimeExceeded
  | ParameterProblem
  | Timestamp
  | TimestampReply
  | InformationRequest
  | InformationReply
with
  EchoReply = 0,
  DestinationUnreachable = 3,
  SourceQuench = 4,
  Redirect = 5,
  EchoRequest = 8,
  RouterAdvertisement = 9,
  RouterSolicitation = 10,
  TimeExceeded = 11,
  ParameterProblem = 12,
  Timestamp = 13,
  TimestampReply = 14,
  InformationRequest = 15,
  InformationReply = 16;

type DestUnreachableCode =
  | NetUnreachable
  | HostUnreachable
  | ProtocolUnreachable
  | PortUnreachable
  | FragmentationNeeded
  | SourceRouteFailed
  | NetworkUnknown
  | HostUnknown
with
  NetUnreachable = 0,
  HostUnreachable = 1,
  ProtocolUnreachable = 2,
  PortUnreachable = 3,
  FragmentationNeeded = 4,
  SourceRouteFailed = 5,
  NetworkUnknown = 6,
  HostUnknown = 7;

type ICMPPacket = {
  type: ICMPType;
  code: match type {
    DestinationUnreachable => DestUnreachableCode
    | _ => uint8
  };
  checksum: uint16;
  payload: match type {
    EchoRequest | EchoReply => {
      identifier: uint16;
      sequence_number: uint16;
      data: bytes
    }
    | Timestamp | TimestampReply => {
      identifier: uint16;
      sequence_number: uint16;
      originate_timestamp: uint32;
      receive_timestamp: uint32;
      transmit_timestamp: uint32
    }
    | ParameterProblem => {
      pointer: uint8;
      unused: uint24;
      original_datagram: bytes
    }
    | TimeExceeded => {
      unused: uint32;
      original_datagram: bytes
    }
    | Redirect => {
      gateway_address: ipv4;
      original_datagram: bytes
    }
    | _ => bytes
  }
};