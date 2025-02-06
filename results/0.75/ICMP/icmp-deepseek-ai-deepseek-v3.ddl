ICMP = struct {
  type: uint8;
  code: uint8;
  checksum: uint16;
  body: switch (type) {
    0 | 8 => Echo {
      identifier: uint16;
      sequence_number: uint16;
      data: bytes;
    };
    3 => DestinationUnreachable {
      unused: uint32;
      original_header: IPHeader;
      original_data: bytes[8];
    };
    4 => SourceQuench {
      unused: uint32;
      original_header: IPHeader;
      original_data: bytes[8];
    };
    5 => Redirect {
      gateway_address: uint32;
      original_header: IPHeader;
      original_data: bytes[8];
    };
    11 => TimeExceeded {
      unused: uint32;
      original_header: IPHeader;
      original_data: bytes[8];
    };
    12 => ParameterProblem {
      pointer: uint8;
      unused: uint24;
      original_header: IPHeader;
      original_data: bytes[8];
    };
    13 | 14 => Timestamp {
      identifier: uint16;
      sequence_number: uint16;
      originate_timestamp: uint32;
      receive_timestamp: uint32;
      transmit_timestamp: uint32;
    };
    15 | 16 => Information {
      identifier: uint16;
      sequence_number: uint16;
    };
    17 | 18 => AddressMask {
      identifier: uint16;
      sequence_number: uint16;
      address_mask: uint32;
    };
    default => Unknown {
      payload: bytes;
    };
  };
};

IPHeader = struct {
  version_and_ihl: uint8;
  tos: uint8;
  total_length: uint16;
  identification: uint16;
  flags_and_fragment_offset: uint16;
  ttl: uint8;
  protocol: uint8;
  checksum: uint16;
  source_address: uint32;
  destination_address: uint32;
  options: bytes[(version_and_ihl & 0x0F) * 4 - 20];
};