format binary {
  packet ARP {
    hardwareType: uint16,
    protocolType: uint16,
    hardwareAddressLength: uint8,
    protocolAddressLength: uint8,
    operation: uint16,
    senderHardwareAddress: bytes,
    senderProtocolAddress: bytes,
    targetHardwareAddress: bytes,
    targetProtocolAddress: bytes
  }
}

enum HardwareType {
  ETHERNET = 1,
  EXPERIMENTAL_ETHERNET = 2,
  AMATEUR_RADIO_AX25 = 3,
  PROTEON_PRONET_TOKEN_RING = 4,
  CHAOS = 5,
  IEEE_802_NETWORKS = 6,
  ARCNET = 7,
  HYPERCHANNEL = 8,
  LANSTAR = 9,
  AUTONET_SHORT_ADDRESS = 10,
  LOCALTALK = 11,
  LOCALNET = 12,
  ULTRA_LINK = 13,
  SMDS = 14,
  FRAME_RELAY = 15,
  ATM = 16,
  HDLC = 17,
  FIBRE_CHANNEL = 18,
  SERIAL_LINE = 20,
  MIL_STD_188_220 = 22,
  METRICOM = 23,
  IEEE_1394_1995 = 24,
  MAPOS = 25,
  TWINAXIAL = 26,
  EUI_64 = 27,
  HIPARP = 28,
  IP_AND_ARP_OVER_ISO_7816_1 = 29,
  ARP_IN_ARPANET = 30,
  IP_AND_ARP_OVER_FIBRE_CHANNEL = 31,
  HIPARP_IP = 32
}

enum ProtocolType {
  IPv4 = 0x0800,
  ARP = 0x0806,
  RARP = 0x0835,
  TRILL = 0x22F3,
  DECNET_PHASE_IV = 0x6003
}

enum Operation {
  REQUEST = 1,
  REPLY = 2,
  REQUEST_REVERSE = 3,
  REPLY_REVERSE = 4
}