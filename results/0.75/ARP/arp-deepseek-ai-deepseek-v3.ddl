arp = {
  htype: uint16be;
  ptype: uint16be;
  hlen: uint8;
  plen: uint8;
  oper: uint16be;
  sha: uint8[hlen];
  spa: uint8[plen];
  tha: uint8[hlen];
  tpa: uint8[plen];
}