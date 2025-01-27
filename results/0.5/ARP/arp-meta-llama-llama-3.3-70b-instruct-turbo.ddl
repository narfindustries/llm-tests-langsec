domain ARP {
  type EthernetAddress = byte[6];
  type HardwareType = uint16;
  type ProtocolType = uint16;
  type Operation = uint16;

  struct ARPMessage {
    field hardwareType: HardwareType;
    field protocolType: ProtocolType;
    field hardwareAddressLength: uint8;
    field protocolAddressLength: uint8;
    field operation: Operation;
    field senderHardwareAddress: EthernetAddress;
    field senderProtocolAddress: byte[4];
    field targetHardwareAddress: EthernetAddress;
    field targetProtocolAddress: byte[4];
  }

  type ARP = ARPMessage;
}