format binary {
  littleEndian = false
}

type uint8 = byte
type uint16 = bytes[2]
type uint32 = bytes[4]

type icmp_type = uint8 {
  echo_reply = 0,
  reserved_1 = 1,
  reserved_2 = 2,
  destination_unreachable = 3,
  source_quench = 4,
  redirect = 5,
  reserved_6 = 6,
  reserved_7 = 7,
  echo_request = 8,
  reserved_9 = 9,
  reserved_10 = 10,
  time_exceeded = 11,
  parameter_problem = 12,
  timestamp_request = 13,
  timestamp_reply = 14,
  information_request = 15,
  information_reply = 16
}

type icmp_code = uint8 {
  network_unreachable = 0 when type == destination_unreachable,
  host_unreachable = 1 when type == destination_unreachable,
  protocol_unreachable = 2 when type == destination_unreachable,
  port_unreachable = 3 when type == destination_unreachable,
  fragmentation_needed = 4 when type == destination_unreachable,
  redirect_datagram_for_network = 0 when type == redirect,
  redirect_datagram_for_host = 1 when type == redirect,
  ttl_exceeded_in_transit = 0 when type == time_exceeded,
  fragment_reassembly_time_exceeded = 1 when type == time_exceeded,
  pointer_indicates_error = 0 when type == parameter_problem
}

type icmp_message = struct {
  type: icmp_type,
  code: icmp_code,
  checksum: uint16,
  body: choice {
    echo_request: struct {
      identifier: uint16,
      sequence_number: uint16
    } when type == echo_request,
    echo_reply: struct {
      identifier: uint16,
      sequence_number: uint16
    } when type == echo_reply,
    destination_unreachable: struct {
      unused: uint32,
      internet_header: bytes[24],
      original_datagram_data: bytes[40]
    } when type == destination_unreachable,
    source_quench: struct {
      unused: uint32,
      internet_header: bytes[24],
      original_datagram_data: bytes[40]
    } when type == source_quench,
    redirect: struct {
      gateway_address: uint32,
      internet_header: bytes[24],
      original_datagram_data: bytes[40]
    } when type == redirect,
    time_exceeded: struct {
      unused: uint32,
      internet_header: bytes[24],
      original_datagram_data: bytes[40]
    } when type == time_exceeded,
    parameter_problem: struct {
      pointer: uint8,
      internet_header: bytes[24],
      original_datagram_data: bytes[40]
    } when type == parameter_problem,
    timestamp_request: struct {
      identifier: uint16,
      sequence_number: uint16,
      originate_timestamp: uint32,
      receive_timestamp: uint32,
      transmit_timestamp: uint32
    } when type == timestamp_request,
    timestamp_reply: struct {
      identifier: uint16,
      sequence_number: uint16,
      originate_timestamp: uint32,
      receive_timestamp: uint32,
      transmit_timestamp: uint32
    } when type == timestamp_reply,
    information_request: struct {
      identifier: uint16,
      sequence_number: uint16
    } when type == information_request,
    information_reply: struct {
      identifier: uint16,
      sequence_number: uint16
    } when type == information_reply
  }
}