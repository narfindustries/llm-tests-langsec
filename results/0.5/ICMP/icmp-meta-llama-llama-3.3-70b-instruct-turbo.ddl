domain icmp {
  types {
    uint8  : 1 byte;
    uint16 : 2 bytes;
    uint32 : 4 bytes;
  }

  type icmp_packet {
    fields {
      type : uint8;
      code : uint8;
      checksum : uint16;
      identifier : uint16;
      sequence_number : uint16;
    }
  }

  type icmp_echo_request {
    extends icmp_packet;
    fields {
      header : type == 8;
      timestamp : uint32;
    }
  }

  type icmp_echo_reply {
    extends icmp_packet;
    fields {
      header : type == 0;
      timestamp : uint32;
    }
  }

  grammar icmp {
    entryPoints {
      start : "icmp_packet";
    }

    icmp_packet {
      type : uint8;
      code : uint8;
      checksum : uint16;
      identifier : uint16;
      sequence_number : uint16;
    }

    icmp_echo_request {
      header : type == 8;
      timestamp : uint32;
    }

    icmp_echo_reply {
      header : type == 0;
      timestamp : uint32;
    }
  }
}