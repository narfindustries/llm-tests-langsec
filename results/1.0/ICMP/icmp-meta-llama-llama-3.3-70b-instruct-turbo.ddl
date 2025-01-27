domain icmp_meta {
  import "std/byteorder.dll";
  import "std/endian.dll";

  type icmp_packet_t {
    uint8 type;
    uint8 code;
    uint16 checksum;
    uint32 rest_of_header;
    bytes payload;
  }

  type icmp_echo_request_t : icmp_packet_t {
    matches /type == 8/;
    assert /code == 0/;
  }

  type icmp_echo_reply_t : icmp_packet_t {
    matches /type == 0/;
    assert /code == 0/;
  }

  type icmp_destination_unreachable_t : icmp_packet_t {
    matches /type == 3/;
    uint8 subtype;
    uint32 unused;
    bytes original_ip_packet;
  }

  type icmp_time_exceeded_t : icmp_packet_t {
    matches /type == 11/;
    uint8 subtype;
    uint32 unused;
    bytes original_ip_packet;
  }

  stream icmp_stream {
    var icmp_packet : icmp_packet_t = read(icmp_packet_t);
    var echo_requests : array[icmp_echo_request_t] = filter(icmp_packet, icmp_echo_request_t);
    var echo_replies : array[icmp_echo_reply_t] = filter(icmp_packet, icmp_echo_reply_t);
    var unreachable : array[icmp_destination_unreachable_t] = filter(icmp_packet, icmp_destination_unreachable_t);
    var time_exceeded : array[icmp_time_exceeded_t] = filter(icmp_packet, icmp_time_exceeded_t);
  }

  action print_icmp_packet(icmp_packet : icmp_packet_t) {
    print(icmp_packet.type);
    print(icmp_packet.code);
    print(icmp_packet.checksum);
    print(icmp_packet.rest_of_header);
    print(icmp_packet.payload);
  }

  action print_icmp_type(icmp_packet : icmp_packet_t) {
    var packet_type : string;
    switch (icmp_packet.type) {
      case 0:
        packet_type = "Echo Reply";
        break;
      case 3:
        packet_type = "Destination Unreachable";
        break;
      case 8:
        packet_type = "Echo Request";
        break;
      case 11:
        packet_type = "Time Exceeded";
        break;
      default:
        packet_type = "Unknown";
    }
    print(packet_type);
  }

  action process_icmp_packet(icmp_packet : icmp_packet_t) {
    print_icmp_packet(icmp_packet);
    print_icmp_type(icmp_packet);
  }

  action main() {
    while (true) {
      var icmp_packet : icmp_packet_t = read(icmp_packet_t);
      process_icmp_packet(icmp_packet);
    }
  }
}