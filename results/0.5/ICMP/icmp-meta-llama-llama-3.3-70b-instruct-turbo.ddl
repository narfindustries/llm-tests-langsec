format binary

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
  17-32 = unassigned
}

type icmp_code = uint8 {
  when parent.type = destination_unreachable {
    0 = network_unreachable,
    1 = host_unreachable,
    2 = protocol_unreachable,
    3 = port_unreachable,
    4 = fragmentation_needed_and_dont_fragment_was_set,
    5 = source_route_failed,
    6-15 = unassigned
  },
  when parent.type = redirect {
    0 = redirect_datagram_for_the_network,
    1 = redirect_datagram_for_the_host,
    2 = redirect_datagram_for_the_type_of_service_and_network,
    3 = redirect_datagram_for_the_type_of_service_and_host,
    4-15 = unassigned
  },
  when parent.type = time_exceeded {
    0 = time_to_live_exceeded_in_transit,
    1 = fragment_reassembly_time_exceeded,
    2-15 = unassigned
  },
  when parent.type = parameter_problem {
    0 = pointer_indicates_the_error,
    1 = missing_a_required_option,
    2-15 = unassigned
  }
}

type icmp_header = struct {
  type: icmp_type,
  code: icmp_code,
  checksum: uint16,
  identifier: uint16,
  sequence_number: uint16
}

type icmp_echo_reply = struct {
  header: icmp_header,
  data: bytes
}

type icmp_destination_unreachable = struct {
  header: icmp_header,
  unused: uint32,
  header_and_first_64_bits_of_original_datagram: bytes
}

type icmp_source_quench = struct {
  header: icmp_header,
  unused: uint32,
  header_and_first_64_bits_of_original_datagram: bytes
}

type icmp_redirect = struct {
  header: icmp_header,
  gateway_address: uint32,
  header_and_first_64_bits_of_original_datagram: bytes
}

type icmp_echo_request = struct {
  header: icmp_header,
  data: bytes
}

type icmp_router_advertisement = struct {
  header: icmp_header,
  num_addresses: uint8,
  address_entry_size: uint8,
  lifetime: uint16,
  addresses: array of struct {
    address: uint32,
    prefix_length: uint8
  }
}

type icmp_router_solicitation = struct {
  header: icmp_header,
  unused: uint32
}

type icmp_time_exceeded = struct {
  header: icmp_header,
  unused: uint32,
  header_and_first_64_bits_of_original_datagram: bytes
}

type icmp_parameter_problem = struct {
  header: icmp_header,
  pointer: uint8,
  header_and_first_64_bits_of_original_datagram: bytes
}

type icmp_timestamp_request = struct {
  header: icmp_header,
  identifier: uint16,
  sequence_number: uint16,
  originate_timestamp: uint32
}

type icmp_timestamp_reply = struct {
  header: icmp_header,
  identifier: uint16,
  sequence_number: uint16,
  originate_timestamp: uint32,
  receive_timestamp: uint32,
  transmit_timestamp: uint32
}

type icmp_information_request = struct {
  header: icmp_header,
  identifier: uint16,
  sequence_number: uint16
}

type icmp_information_reply = struct {
  header: icmp_header,
  identifier: uint16,
  sequence_number: uint16
}

type icmp_message = choice {
  echo_reply: icmp_echo_reply,
  destination_unreachable: icmp_destination_unreachable,
  source_quench: icmp_source_quench,
  redirect: icmp_redirect,
  echo_request: icmp_echo_request,
  router_advertisement: icmp_router_advertisement,
  router_solicitation: icmp_router_solicitation,
  time_exceeded: icmp_time_exceeded,
  parameter_problem: icmp_parameter_problem,
  timestamp_request: icmp_timestamp_request,
  timestamp_reply: icmp_timestamp_reply,
  information_request: icmp_information_request,
  information_reply: icmp_information_reply
}