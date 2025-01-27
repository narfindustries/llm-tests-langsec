def Main = {
  def IPv4Header = {
    version : uint8;
    ihl : uint8;
    tos : uint8;
    total_length : uint16;
    id : uint16;
    flags_fragment : uint16;
    ttl : uint8;
    protocol : uint8;
    header_checksum : uint16;
    src_addr : uint32;
    dst_addr : uint32
  }

  def ICMPHeader = {
    type : uint8;
    code : uint8;
    checksum : uint16;
    rest_of_header : uint32
  }

  def ICMPEchoData = {
    identifier : uint16;
    sequence : uint16;
    data : Array uint8
  }

  def ICMPPacket = {
    ipv4 : IPv4Header;
    icmp : ICMPHeader;
    @if (icmp.type == 0 || icmp.type == 8)
      echo_data : ICMPEchoData
  }

  ICMPPacket
}