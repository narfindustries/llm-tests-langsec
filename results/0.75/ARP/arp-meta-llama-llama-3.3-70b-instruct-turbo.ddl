domain arp {
  import binary;

  type EthernetAddress = byte[6];
  type HardwareType = uint16;
  type ProtocolType = uint16;
  type Operation = uint16;

  type ARPFrame {
    hardware_type: HardwareType,
    protocol_type: ProtocolType,
    hardware_address_length: uint8,
    protocol_address_length: uint8,
    operation: Operation,
    sender_hardware_address: EthernetAddress,
    sender_protocol_address: byte[4],
    target_hardware_address: EthernetAddress,
    target_protocol_address: byte[4],
  }

  constraint valid_arp_frame {
    hardware_type == 1, // Ethernet
    protocol_type == 2048, // IPv4
    hardware_address_length == 6, // Ethernet address length
    protocol_address_length == 4, // IPv4 address length
    operation == 1 || operation == 2, // Request or Reply
  }

  parse arp_frame(): ARPFrame {
    hardware_type: parse uint16(),
    protocol_type: parse uint16(),
    hardware_address_length: parse uint8(),
    protocol_address_length: parse uint8(),
    operation: parse uint16(),
    sender_hardware_address: parse byte[6],
    sender_protocol_address: parse byte[4],
    target_hardware_address: parse byte[6],
    target_protocol_address: parse byte[4],
  }
}