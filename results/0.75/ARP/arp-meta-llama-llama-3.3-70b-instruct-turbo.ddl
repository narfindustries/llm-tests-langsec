seq arp {
  uint16 hardware_type;
  uint16 protocol_type;
  uint8 hardware_address_length;
  uint8 protocol_address_length;
  uint16 operation;
  bytes sender_hardware_address;
  bytes sender_protocol_address;
  bytes target_hardware_address;
  bytes target_protocol_address;
}

enum HardwareType {
  Ethernet = 1;
  ExperimentalEthernet = 2;
  AmateurRadioAX25 = 3;
  ProteonProNETTokenRing = 4;
  Chaos = 5;
  IEEE802Networks = 6;
  ARCNET = 7;
  Hyperchannel = 8;
  Lanstar = 9;
  AutonetShortAddress = 10;
  LocalTalk = 11;
  LocalNet = 12;
  UltraLink = 13;
  SMDS = 14;
  FrameRelay = 15;
  ATM = 16;
  HDLC = 17;
  FibreChannel = 18;
  ATM2 = 19;
  SerialLine = 20;
  ATM3 = 21;
  MILSTD188220 = 22;
  Metricom = 23;
  IEEE13941995 = 24;
  MAPOS = 25;
  Twinaxial = 26;
  EUI64 = 27;
  HIPARP = 28;
  IPandARPOverISO10589LLC = 29;
  ARPinARPANET = 30;
  IPandARPOnTheWire = 31;
}

enum ProtocolType {
  IPv4 = 0x0800;
  ARP = 0x0806;
  RARP = 0x8035;
}

enum Operation {
  Request = 1;
  Reply = 2;
  RequestReverse = 3;
  ReplyReverse = 4;
}