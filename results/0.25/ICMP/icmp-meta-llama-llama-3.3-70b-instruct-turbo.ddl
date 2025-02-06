format icmp {
  type: uint8,
  code: uint8,
  checksum: uint16,
  identifier: uint16,
  sequence_number: uint16
}

format icmp_echo {
  header: icmp,
  data: bytes
}

format icmp_destination_unreachable {
  header: icmp,
  unused: uint32,
  ip_header: bytes
}

format icmp_source_quench {
  header: icmp,
  unused: uint32,
  ip_header: bytes
}

format icmp_redirect {
  header: icmp,
  gateway_address: uint32,
  ip_header: bytes
}

format icmp_time_exceeded {
  header: icmp,
  unused: uint32,
  ip_header: bytes
}

format icmp_parameter_problem {
  header: icmp,
  pointer: uint8,
  unused: uint32,
  ip_header: bytes
}

format icmp_timestamp {
  header: icmp,
  originate_timestamp: uint32,
  receive_timestamp: uint32,
  transmit_timestamp: uint32
}

format icmp_timestamp_reply {
  header: icmp,
  originate_timestamp: uint32,
  receive_timestamp: uint32,
  transmit_timestamp: uint32
}

format icmp_information_request {
  header: icmp,
  identifier: uint16,
  sequence_number: uint16
}

format icmp_information_reply {
  header: icmp,
  identifier: uint16,
  sequence_number: uint16
}

format icmp_message {
  switch (type) {
    case 0: echo_reply: icmp_echo,
    case 3: destination_unreachable: icmp_destination_unreachable,
    case 4: source_quench: icmp_source_quench,
    case 5: redirect: icmp_redirect,
    case 11: time_exceeded: icmp_time_exceeded,
    case 12: parameter_problem: icmp_parameter_problem,
    case 13: timestamp: icmp_timestamp,
    case 14: timestamp_reply: icmp_timestamp_reply,
    case 15: information_request: icmp_information_request,
    case 16: information_reply: icmp_information_reply,
    case 8: echo: icmp_echo,
    case 9: router_advertisement: bytes,
    case 10: router_solicitation: bytes,
    case 6: alternate_host_address: bytes
  }
}