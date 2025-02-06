icmp {
  type: uint8;
  code: uint8;
  checksum: uint16;
  rest_of_header: switch (type) {
    case 0: echo_reply {
      identifier: uint16;
      sequence_number: uint16;
    };
    case 8: echo_request {
      identifier: uint16;
      sequence_number: uint16;
    };
    case 3: destination_unreachable {
      unused: uint32;
      original_datagram: bytes[64];
    };
    case 5: redirect {
      gateway_internet_address: uint32;
      original_datagram: bytes[64];
    };
    case 11: time_exceeded {
      unused: uint32;
      original_datagram: bytes[64];
    };
    case 12: parameter_problem {
      pointer: uint8;
      unused: uint24;
      original_datagram: bytes[64];
    };
    case 13: timestamp_request {
      identifier: uint16;
      sequence_number: uint16;
      originate_timestamp: uint32;
      receive_timestamp: uint32;
      transmit_timestamp: uint32;
    };
    case 14: timestamp_reply {
      identifier: uint16;
      sequence_number: uint16;
      originate_timestamp: uint32;
      receive_timestamp: uint32;
      transmit_timestamp: uint32;
    };
    case 17: address_mask_request {
      identifier: uint16;
      sequence_number: uint16;
      address_mask: uint32;
    };
    case 18: address_mask_reply {
      identifier: uint16;
      sequence_number: uint16;
      address_mask: uint32;
    };
    default: unknown {
      data: bytes[];
    };
  };
}