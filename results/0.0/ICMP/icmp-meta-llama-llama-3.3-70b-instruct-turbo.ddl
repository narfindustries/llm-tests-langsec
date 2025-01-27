domain icmp {
  import std.all;

  type icmp_packet {
    uint8 type;
    uint8 code;
    uint16 checksum;
    uint32 rest_of_header;
    bytes payload;
  }

  type icmp_echo_request {
    extends icmp_packet;
    constraint type == 8;
    constraint code == 0;
  }

  type icmp_echo_reply {
    extends icmp_packet;
    constraint type == 0;
    constraint code == 0;
  }

  type icmp_destination_unreachable {
    extends icmp_packet;
    constraint type == 3;
  }

  type icmp_source_quench {
    extends icmp_packet;
    constraint type == 4;
  }

  type icmp_redirect {
    extends icmp_packet;
    constraint type == 5;
  }

  type icmp_time_exceeded {
    extends icmp_packet;
    constraint type == 11;
  }

  type icmp_parameter_problem {
    extends icmp_packet;
    constraint type == 12;
  }

  type icmp_timestamp {
    extends icmp_packet;
    constraint type == 13;
  }

  type icmp_timestamp_reply {
    extends icmp_packet;
    constraint type == 14;
  }

  type icmp_info_request {
    extends icmp_packet;
    constraint type == 15;
  }

  type icmp_info_reply {
    extends icmp_packet;
    constraint type == 16;
  }

  type icmp_address_mask_request {
    extends icmp_packet;
    constraint type == 17;
  }

  type icmp_address_mask_reply {
    extends icmp_packet;
    constraint type == 18;
  }

  grammar icmp_packet {
    type: uint8;
    code: uint8;
    checksum: uint16;
    rest_of_header: uint32;
    payload: bytes;
  }

  grammar icmp_echo_request {
    type: uint8 = 8;
    code: uint8 = 0;
    checksum: uint16;
    rest_of_header: uint32;
    payload: bytes;
  }

  grammar icmp_echo_reply {
    type: uint8 = 0;
    code: uint8 = 0;
    checksum: uint16;
    rest_of_header: uint32;
    payload: bytes;
  }

  grammar icmp_destination_unreachable {
    type: uint8 = 3;
    code: uint8;
    checksum: uint16;
    rest_of_header: uint32;
    payload: bytes;
  }

  grammar icmp_source_quench {
    type: uint8 = 4;
    code: uint8;
    checksum: uint16;
    rest_of_header: uint32;
    payload: bytes;
  }

  grammar icmp_redirect {
    type: uint8 = 5;
    code: uint8;
    checksum: uint16;
    rest_of_header: uint32;
    payload: bytes;
  }

  grammar icmp_time_exceeded {
    type: uint8 = 11;
    code: uint8;
    checksum: uint16;
    rest_of_header: uint32;
    payload: bytes;
  }

  grammar icmp_parameter_problem {
    type: uint8 = 12;
    code: uint8;
    checksum: uint16;
    rest_of_header: uint32;
    payload: bytes;
  }

  grammar icmp_timestamp {
    type: uint8 = 13;
    code: uint8;
    checksum: uint16;
    rest_of_header: uint32;
    payload: bytes;
  }

  grammar icmp_timestamp_reply {
    type: uint8 = 14;
    code: uint8;
    checksum: uint16;
    rest_of_header: uint32;
    payload: bytes;
  }

  grammar icmp_info_request {
    type: uint8 = 15;
    code: uint8;
    checksum: uint16;
    rest_of_header: uint32;
    payload: bytes;
  }

  grammar icmp_info_reply {
    type: uint8 = 16;
    code: uint8;
    checksum: uint16;
    rest_of_header: uint32;
    payload: bytes;
  }

  grammar icmp_address_mask_request {
    type: uint8 = 17;
    code: uint8;
    checksum: uint16;
    rest_of_header: uint32;
    payload: bytes;
  }

  grammar icmp_address_mask_reply {
    type: uint8 = 18;
    code: uint8;
    checksum: uint16;
    rest_of_header: uint32;
    payload: bytes;
  }
}