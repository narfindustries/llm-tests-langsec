grammar ICMP {
  message IcmpMessage {
    uint8 type;
    uint8 code;
    uint16 checksum;
    uint16 identifier;
    uint16 sequenceNumber;
    bytes data;
  }
}
