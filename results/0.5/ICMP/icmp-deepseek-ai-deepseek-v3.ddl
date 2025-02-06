ICMP = {
  type: uint8,
  code: uint8,
  checksum: uint16,
  rest: switch (type) {
    0: EchoReply = {
      identifier: uint16,
      sequence_number: uint16,
      data: bytes
    },
    3: DestinationUnreachable = {
      unused: uint32,
      original_datagram: bytes
    },
    4: SourceQuench = {
      unused: uint32,
      original_datagram: bytes
    },
    5: Redirect = {
      gateway_address: uint32,
      original_datagram: bytes
    },
    8: EchoRequest = {
      identifier: uint16,
      sequence_number: uint16,
      data: bytes
    },
    11: TimeExceeded = {
      unused: uint32,
      original_datagram: bytes
    },
    12: ParameterProblem = {
      pointer: uint8,
      unused: uint24,
      original_datagram: bytes
    },
    13: TimestampRequest = {
      identifier: uint16,
      sequence_number: uint16,
      originate_timestamp: uint32,
      receive_timestamp: uint32,
      transmit_timestamp: uint32
    },
    14: TimestampReply = {
      identifier: uint16,
      sequence_number: uint16,
      originate_timestamp: uint32,
      receive_timestamp: uint32,
      transmit_timestamp: uint32
    },
    15: InformationRequest = {
      identifier: uint16,
      sequence_number: uint16
    },
    16: InformationReply = {
      identifier: uint16,
      sequence_number: uint16
    },
    17: AddressMaskRequest = {
      identifier: uint16,
      sequence_number: uint16,
      address_mask: uint32
    },
    18: AddressMaskReply = {
      identifier: uint16,
      sequence_number: uint16,
      address_mask: uint32
    }
  }
};