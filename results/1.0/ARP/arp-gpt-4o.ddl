module arp-gpt-4o {

  enum ArpOperation : uint16 {
    REQUEST = 1,
    REPLY = 2
  }

  struct ArpHeader {
    hardware_type: uint16;
    protocol_type: uint16;
    hardware_size: uint8;
    protocol_size: uint8;
    operation: ArpOperation;
    sender_hardware_address: string(6); // 6 bytes for MAC
    sender_protocol_address: ipv4;      // Assuming IPv4
    target_hardware_address: string(6); // 6 bytes for MAC
    target_protocol_address: ipv4;      // Assuming IPv4
  }

  alias ArpPacket = ArpHeader;
}