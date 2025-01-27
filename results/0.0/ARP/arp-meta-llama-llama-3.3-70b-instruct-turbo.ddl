domain ARP {
  type EthernetAddress = byte[6];
  type HardwareType = uint16;
  type ProtocolType = uint16;
  type Operation = uint16;

  struct ARPMessage {
    hardware_type: HardwareType,
    protocol_type: ProtocolType,
    hardware_address_length: uint8,
    protocol_address_length: uint8,
    operation: Operation,
    sender_hardware_address: EthernetAddress,
    sender_protocol_address: byte[4],
    target_hardware_address: EthernetAddress,
    target_protocol_address: byte[4]
  }

  root_type ARPMessage;
}