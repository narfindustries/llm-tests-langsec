type arp_header {
  uint16 htype;
  uint16 ptype;
  uint8 hlen;
  uint8 plen;
  uint16 opcode;
  bytes sha[hlen];
  bytes spa[plen];
  bytes tha[hlen];
  bytes tpa[plen];
}
