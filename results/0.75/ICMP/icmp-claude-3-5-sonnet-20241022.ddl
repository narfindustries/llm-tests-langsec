def Main = {
  def ICMP_header = {
    type       : uint 8;
    code       : uint 8;
    checksum   : uint 16;
    identifier : uint 16;
    sequence   : uint 16
  }

  def ICMP_payload = {
    data : uint 8[]
  }

  def ICMP_packet = {
    header  : ICMP_header;
    payload : ICMP_payload
  }

  ICMP_packet
}