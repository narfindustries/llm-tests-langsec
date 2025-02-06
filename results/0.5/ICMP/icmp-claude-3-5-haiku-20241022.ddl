enum ICMPType {
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
  InformationReply = 16
}

enum ICMPDestUnreachableCode {
  NetUnreachable = 0,
  HostUnreachable = 1,
  ProtocolUnreachable = 2,
  PortUnreachable = 3,
  FragmentationNeeded = 4,
  SourceRouteFailed = 5
}

enum ICMPRedirectCode {
  NetworkRedirect = 0,
  HostRedirect = 1,
  NetworkTypeOfServiceRedirect = 2,
  HostTypeOfServiceRedirect = 3
}

enum ICMPTimeExceededCode {
  TTLExpired = 0,
  FragmentReassemblyTimeExceeded = 1
}

enum ICMPParameterProblemCode {
  PointerIndicatesError = 0,
  MissingRequiredOption = 1,
  BadLength = 2
}

struct ICMPHeader {
  type: ICMPType,
  code: uint8,
  checksum: uint16,
  rest_of_header: variant type {
    case EchoReply: {
      identifier: uint16,
      sequence_number: uint16,
      data: bytes
    },
    case EchoRequest: {
      identifier: uint16,
      sequence_number: uint16,
      data: bytes
    },
    case DestinationUnreachable: {
      unused: uint32,
      original_datagram: bytes
    },
    case SourceQuench: {
      unused: uint32,
      original_datagram: bytes
    },
    case Redirect: {
      gateway_address: uint32,
      original_datagram: bytes
    },
    case RouterAdvertisement: {
      number_of_addresses: uint8,
      address_entry_size: uint8,
      lifetime: uint16,
      addresses: bytes
    },
    case RouterSolicitation: {
      reserved: uint32
    },
    case TimeExceeded: {
      unused: uint32,
      original_datagram: bytes
    },
    case ParameterProblem: {
      pointer: uint8,
      unused: uint24,
      original_datagram: bytes
    },
    case Timestamp: {
      identifier: uint16,
      sequence_number: uint16,
      originate_timestamp: uint32,
      receive_timestamp: uint32,
      transmit_timestamp: uint32
    },
    case TimestampReply: {
      identifier: uint16,
      sequence_number: uint16,
      originate_timestamp: uint32,
      receive_timestamp: uint32,
      transmit_timestamp: uint32
    },
    case InformationRequest: {
      identifier: uint16,
      sequence_number: uint16
    },
    case InformationReply: {
      identifier: uint16,
      sequence_number: uint16
    }
  }
}