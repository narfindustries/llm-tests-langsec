ICMP {
  type: UInt8;
  code: UInt8;
  checksum: UInt16;
  rest_of_header: switch (type) {
    case 0 | 8: {
      identifier: UInt16;
      sequence_number: UInt16;
    };
    case 3: {
      unused: UInt32;
    };
    case 4: {
      unused: UInt32;
    };
    case 5: {
      gateway_internet_address: UInt32;
    };
    case 9 | 10: {
      unused: UInt32;
    };
    case 11: {
      unused: UInt32;
    };
    case 12: {
      pointer: UInt8;
      unused: UInt24;
    };
    case 13 | 14: {
      identifier: UInt16;
      sequence_number: UInt16;
      originate_timestamp: UInt32;
      receive_timestamp: UInt32;
      transmit_timestamp: UInt32;
    };
    case 15 | 16: {
      identifier: UInt16;
      sequence_number: UInt16;
    };
    default: {
      unused: UInt32;
    };
  };
  data: Bytes;
}