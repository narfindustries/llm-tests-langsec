def Main = (
  P_IPv4  -- IPv4 header
  ICMP    -- ICMP message
)

def P_IPv4 = {
  version : uint 4 where version == 4;
  ihl : uint 4;
  service : uint 8;
  total_length : uint 16;
  id : uint 16;
  flags : uint 3;
  frag_offset : uint 13;
  ttl : uint 8;
  protocol : uint 8 where protocol == 1; -- ICMP protocol number
  header_checksum : uint 16;
  src_addr : uint 32;
  dst_addr : uint 32
}

def ICMP = {
  type : uint 8;
  code : uint 8;
  checksum : uint 16;
  
  rest : case type of {
    0 -> Echo_Reply
    8 -> Echo_Request
    3 -> Destination_Unreachable
    11 -> Time_Exceeded
    _ -> Unknown_ICMP
  }
}

def Echo_Reply = {
  identifier : uint 16;
  sequence : uint 16;
  data : bytes
}

def Echo_Request = {
  identifier : uint 16;
  sequence : uint 16;
  data : bytes
}

def Destination_Unreachable = {
  unused : uint 32;
  ip_header : P_IPv4;
  original_datagram : bytes
}

def Time_Exceeded = {
  unused : uint 32;
  ip_header : P_IPv4;
  original_datagram : bytes
}

def Unknown_ICMP = {
  remaining_data : bytes
}