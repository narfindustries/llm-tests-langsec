domain arp {
  include "types.ddl";

  struct ethernet_frame {
    byte[6] destination_mac;
    byte[6] source_mac;
    ushort ethertype;
    switch (ethertype) {
      case 0x0806: arp_packet arp;
      default: byte[] payload;
    }
  }

  struct arp_packet {
    ushort hardware_type;
    ushort protocol_type;
    byte hardware_address_length;
    byte protocol_address_length;
    ushort opcode;
    byte[hardware_address_length] sender_hardware_address;
    byte[protocol_address_length] sender_protocol_address;
    byte[hardware_address_length] target_hardware_address;
    byte[protocol_address_length] target_protocol_address;
  }
}

transform arp_transform {
  input ethernet_frame frame;
  output arp_packet arp;

  arp = frame.arp;
}

validate arp_validator {
  input arp_packet arp;

  assert arp.hardware_type == 1;
  assert arp.protocol_type == 0x0800;
  assert arp.hardware_address_length == 6;
  assert arp.protocol_address_length == 4;
  assert arp.opcode == 1 || arp.opcode == 2;
}

serialize arp_serializer {
  input arp_packet arp;
  output byte[] bytes;

  bytes = [
    arp.hardware_type,
    arp.protocol_type,
    arp.hardware_address_length,
    arp.protocol_address_length,
    arp.opcode,
    arp.sender_hardware_address,
    arp.sender_protocol_address,
    arp.target_hardware_address,
    arp.target_protocol_address
  ];
}

deserialize arp_deserializer {
  input byte[] bytes;
  output arp_packet arp;

  arp.hardware_type = bytes[0..1];
  arp.protocol_type = bytes[2..3];
  arp.hardware_address_length = bytes[4];
  arp.protocol_address_length = bytes[5];
  arp.opcode = bytes[6..7];
  arp.sender_hardware_address = bytes[8..13];
  arp.sender_protocol_address = bytes[14..17];
  arp.target_hardware_address = bytes[18..23];
  arp.target_protocol_address = bytes[24..27];
}