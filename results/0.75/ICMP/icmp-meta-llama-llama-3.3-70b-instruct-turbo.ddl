domain icmp {
  import all;

  include "base.ddl";

  type ICMP = struct {
    type: uint8,
    code: uint8,
    checksum: uint16,
    identifier: uint16,
    sequence_number: uint16,
  };

  type ICMP_ECHO_REQUEST = struct {
    extend ICMP;
    data: bytes;
  };

  type ICMP_ECHO_REPLY = struct {
    extend ICMP;
    data: bytes;
  };

  type ICMP_DESTINATION_UNREACHABLE = struct {
    extend ICMP;
    unused: uint32,
    data: bytes,
  };

  type ICMP_REDIRECT = struct {
    extend ICMP;
    gateway_address: ipv4,
    data: bytes,
  };

  type ICMP_SOURCE_QUENCH = struct {
    extend ICMP;
    unused: uint32,
    data: bytes,
  };

  type ICMP_ROUTER_ADVERTISEMENT = struct {
    extend ICMP;
    num_addresses: uint8,
    address_entry_size: uint8,
    lifetime: uint16,
    router_addresses: array[ipv4],
    data: bytes,
  };

  type ICMP_ROUTER_SOLICITATION = struct {
    extend ICMP;
    unused: uint32,
    data: bytes,
  };

  type ICMP_TIME_EXCEEDED = struct {
    extend ICMP;
    unused: uint32,
    data: bytes,
  };

  type ICMP_PARAMETER_PROBLEM = struct {
    extend ICMP;
    pointer: uint8,
    unused: uint32,
    data: bytes,
  };

  type ICMP_TIMESTAMP_REQUEST = struct {
    extend ICMP;
    identifier: uint16,
    sequence_number: uint16,
    originate_timestamp: uint32,
    receive_timestamp: uint32,
    transmit_timestamp: uint32,
  };

  type ICMP_TIMESTAMP_REPLY = struct {
    extend ICMP;
    identifier: uint16,
    sequence_number: uint16,
    originate_timestamp: uint32,
    receive_timestamp: uint32,
    transmit_timestamp: uint32,
  };

  type ICMP_INFO_REQUEST = struct {
    extend ICMP;
    identifier: uint16,
    sequence_number: uint16,
  };

  type ICMP_INFO_REPLY = struct {
    extend ICMP;
    identifier: uint16,
    sequence_number: uint16,
  };

  type ICMP_ADDRESS_MASK_REQUEST = struct {
    extend ICMP;
    identifier: uint16,
    sequence_number: uint16,
  };

  type ICMP_ADDRESS_MASK_REPLY = struct {
    extend ICMP;
    identifier: uint16,
    sequence_number: uint16,
  };

  grammar top {
    pattern {
      type == 0 => ICMP_ECHO_REPLY,
      type == 3 => ICMP_DESTINATION_UNREACHABLE,
      type == 4 => ICMP_SOURCE_QUENCH,
      type == 5 => ICMP_REDIRECT,
      type == 8 => ICMP_ECHO_REQUEST,
      type == 9 => ICMP_ROUTER_ADVERTISEMENT,
      type == 10 => ICMP_ROUTER_SOLICITATION,
      type == 11 => ICMP_TIME_EXCEEDED,
      type == 12 => ICMP_PARAMETER_PROBLEM,
      type == 13 => ICMP_TIMESTAMP_REQUEST,
      type == 14 => ICMP_TIMESTAMP_REPLY,
      type == 15 => ICMP_INFO_REQUEST,
      type == 16 => ICMP_INFO_REPLY,
      type == 17 => ICMP_ADDRESS_MASK_REQUEST,
      type == 18 => ICMP_ADDRESS_MASK_REPLY,
    }
  }
}