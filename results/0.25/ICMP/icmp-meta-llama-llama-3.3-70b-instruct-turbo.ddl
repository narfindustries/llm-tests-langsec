domain icmp {
  import "basic_types.ddl";

  type int8  = int<8>;
  type int16 = int<16>;
  type int32 = int<32>;

  type icmp_header = struct {
    type:  int8;
    code:  int8;
    checksum: int16;
    identifier: int16;
    sequence_number: int16;
  };

  type icmp_echo_request = struct {
    header: icmp_header;
    data: byte[];
  };

  type icmp_echo_reply = struct {
    header: icmp_header;
    data: byte[];
  };

  syntax icmp_packet = choice {
    echo_request: icmp_echo_request,
    echo_reply:   icmp_echo_reply
  };
}