type ICMP = struct {
  type : uint8;
  code : uint8;
  checksum : uint16;
  data : bytes;
}