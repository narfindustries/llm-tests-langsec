domain ARP {
  uint8 hardware_type = 1;
  uint8 protocol_type = 0x0800;
  uint16 hardware_length = 6;
  uint16 protocol_length = 4;
  uint16 opcode = 1;
  uint8 sender_mac[6];
  uint32 sender_ip;
  uint8 target_mac[6];
  uint32 target_ip;
}

domain Ethernet {
  uint8 destination_mac[6];
  uint8 source_mac[6];
  uint16 ethertype;
  ARP arp;
}

grammar ARP {
  start = Ethernet;
}

grammar Ethernet {
  start = destination_mac, source_mac, ethertype, arp;
}

grammar bytes_to_domain {
  start = ARP;
}