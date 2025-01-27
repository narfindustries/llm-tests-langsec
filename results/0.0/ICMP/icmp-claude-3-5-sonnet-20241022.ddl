def Main = {
  def IPv4Header = {
    version : uint8;
    IHL : uint8;
    DSCP_ECN : uint8;
    totalLength : uint16;
    identification : uint16;
    flags_fragOffset : uint16;
    TTL : uint8;
    protocol : uint8;
    headerChecksum : uint16;
    sourceIP : uint32;
    destIP : uint32
  }

  def ICMPHeader = {
    type : uint8;
    code : uint8;
    checksum : uint16;
    restOfHeader : uint32
  }

  def ICMPEcho = {
    identifier : uint16;
    sequenceNumber : uint16;
    data : uint8[]
  }

  def ICMPDestUnreach = {
    unused : uint32;
    ipHeader : IPv4Header;
    originalDatagramStart : uint64
  }

  def ICMPTimeExceeded = {
    unused : uint32;
    ipHeader : IPv4Header;
    originalDatagramStart : uint64
  }

  ipHeader = IPv4Header;
  icmpHeader = ICMPHeader;

  case icmpHeader.type of {
    0 => ICMPEcho  -- Echo Reply
    3 => ICMPDestUnreach  -- Destination Unreachable
    8 => ICMPEcho  -- Echo Request
    11 => ICMPTimeExceeded  -- Time Exceeded
    _ => null
  }
}