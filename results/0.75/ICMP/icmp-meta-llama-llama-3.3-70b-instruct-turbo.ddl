format binary {
  struct icmp_header {
    type: uint8,
    code: uint8,
    checksum: uint16,
  }

  struct echo {
    identifier: uint16,
    sequence_number: uint16,
  }

  struct timestamp {
    identifier: uint16,
    sequence_number: uint16,
    timestamp: uint32,
  }

  struct information {
    identifier: uint16,
    sequence_number: uint16,
  }

  struct address_mask {
    identifier: uint16,
    sequence_number: uint16,
    address_mask: uint32,
  }

  struct redirect {
    gateway_address: uint32,
  }

  struct destination_unreachable {
    unused: uint32,
    ip_header: bytes[0..64],
  }

  struct source_quench {
    unused: uint32,
    ip_header: bytes[0..64],
  }

  struct time_exceeded {
    unused: uint32,
    ip_header: bytes[0..64],
  }

  struct parameter_problem {
    pointer: uint8,
    ip_header: bytes[0..64],
  }

  struct icmp_message {
    header: icmp_header,
    body: choice {
      0x00: echo,
      0x08: echo,
      0x13: timestamp,
      0x14: timestamp,
      0x15: information,
      0x16: information,
      0x05: redirect,
      0x03: destination_unreachable,
      0x04: source_quench,
      0x0b: time_exceeded,
      0x0c: parameter_problem,
      default: bytes[0..1024],
    }
  }
}