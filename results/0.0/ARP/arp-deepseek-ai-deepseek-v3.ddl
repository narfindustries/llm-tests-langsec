type ARP_Packet = struct {
  htype: uint16;
  ptype: uint16;
  hlen: uint8;
  plen: uint8;
  oper: uint16;
  sha: bytes[hlen];
  spa: bytes[plen];
  tha: bytes[hlen];
  tpa: bytes[plen];
};