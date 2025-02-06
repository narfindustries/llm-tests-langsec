arp {
  htype: UInt16;
  ptype: UInt16;
  hlen: UInt8;
  plen: UInt8;
  oper: UInt16;
  sha: Bytes[this.hlen];
  spa: Bytes[this.plen];
  tha: Bytes[this.hlen];
  tpa: Bytes[this.plen];
}