arp_spec : struct {
  htype : UInt16,
  ptype : UInt16,
  hlen : UInt8,
  plen : UInt8,
  oper : UInt16,
  sha : Bytes[hlen],
  spa : Bytes[plen],
  tha : Bytes[hlen],
  tpa : Bytes[plen]
}