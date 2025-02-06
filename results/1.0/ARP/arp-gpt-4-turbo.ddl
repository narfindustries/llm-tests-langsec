type HardwareType : uint16 {
  Ethernet = 1,
  IEEE802 = 6
}

type ProtocolType : uint16 {
  IPv4 = 0x0800
}

type Operation : uint16 {
  Request = 1,
  Reply = 2
}

struct ArpPacket {
  htype : HardwareType;
  ptype : ProtocolType;
  hlen  : uint8;
  plen  : uint8;
  op    : Operation;
  sha   : bitvec[8 * hlen];
  spa   : bitvec[8 * plen];
  tha   : bitvec[8 * hlen];
  tpa   : bitvec[8 * plen];
}