type ICMP = struct {
  type: uint8,
  code: uint8,
  checksum: uint16,
  body: switch (type) {
    case 0, 8: struct {
      identifier: uint16,
      sequence_number: uint16,
    },
    case 3: struct {
      unused: uint32,
      original_datagram: byte[],
    },
    case 4: struct {
      unused: uint32,
      original_datagram: byte[],
    },
    case 5: struct {
      gateway_address: uint32,
      original_datagram: byte[],
    },
    case 11: struct {
      unused: uint32,
      original_datagram: byte[],
    },
    case 12: struct {
      pointer: uint8,
      unused: uint24,
      original_datagram: byte[],
    },
    case 13, 14: struct {
      identifier: uint16,
      sequence_number: uint16,
      originate_timestamp: uint32,
      receive_timestamp: uint32,
      transmit_timestamp: uint32,
    },
    case 17, 18: struct {
      identifier: uint16,
      sequence_number: uint16,
      address_mask: uint32,
    },
    default: byte[],
  },
};