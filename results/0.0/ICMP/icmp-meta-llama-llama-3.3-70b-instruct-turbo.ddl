meta {
  name: icmp
  description: Internet Control Message Protocol
}

type icmp_type = uint8 {
  0 = echo_reply,
  1 = unassigned,
  2 = unassigned,
  3 = destination_unreachable,
  4 = source_quench,
  5 = redirect,
  6 = alternate_host_address,
  7 = unassigned,
  8 = echo_request,
  9 = router_advertisement,
  10 = router_solicitation,
  11 = time_exceeded,
  12 = parameter_problem,
  13 = timestamp_request,
  14 = timestamp_reply,
  15 = information_request,
  16 = information_reply,
  17-32 = unassigned,
  33-255 = reserved
}

type icmp_code = uint8 {
  0 = network_unreachable,
  1 = host_unreachable,
  2 = protocol_unreachable,
  3 = port_unreachable,
  4 = fragmentation_needed_and_dont_fragment_was_set,
  5 = source_route_failed,
  6-15 = unassigned
}

type icmp_message = struct {
  type: icmp_type,
  code: icmp_code,
  checksum: uint16,
  identifier: uint16,
  sequence_number: uint16,
  data: choice {
    echo_reply: struct {
      header: struct {
        identifier: uint16,
        sequence_number: uint16
      },
      data: bytes
    },
    echo_request: struct {
      header: struct {
        identifier: uint16,
        sequence_number: uint16
      },
      data: bytes
    },
    destination_unreachable: struct {
      unused: uint32,
      internet_header: bytes
    },
    source_quench: struct {
      unused: uint32,
      internet_header: bytes
    },
    redirect: struct {
      gateway_address: uint32,
      internet_header: bytes
    },
    time_exceeded: struct {
      unused: uint32,
      internet_header: bytes
    },
    parameter_problem: struct {
      pointer: uint8,
      internet_header: bytes
    },
    timestamp_request: struct {
      identifier: uint16,
      sequence_number: uint16,
      originate_timestamp: uint32,
      receive_timestamp: uint32,
      transmit_timestamp: uint32
    },
    timestamp_reply: struct {
      identifier: uint16,
      sequence_number: uint16,
      originate_timestamp: uint32,
      receive_timestamp: uint32,
      transmit_timestamp: uint32
    },
    information_request: struct {
      identifier: uint16,
      sequence_number: uint16
    },
    information_reply: struct {
      identifier: uint16,
      sequence_number: uint16
    },
    router_advertisement: struct {
      num_addrs: uint8,
      addr_entry_size: uint8,
      lifetime: uint16,
      addr_entries: array of struct {
        address: uint32,
        address_mask: uint32
      }
    },
    router_solicitation: struct {
      reserved: uint32
    }
  }
}