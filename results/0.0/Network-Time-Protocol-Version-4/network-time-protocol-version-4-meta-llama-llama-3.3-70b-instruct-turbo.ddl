domain NetworkTimeProtocolVersion4 {
  type NTPPacket {
    uint32 leap: 2;
    uint32 version: 3;
    uint32 mode: 3;
    uint32 poll: 4;
    uint32 precision: 8;
    uint32 delay: 32;
    uint32 dispersion: 32;
    uint32 identifier: 32;
    uint32 referenceTimestamp: 64;
    uint32 originateTimestamp: 64;
    uint32 receiveTimestamp: 64;
    uint32 transmitTimestamp: 64;
  }

  type NTPHeader {
    NTPPacket packet;
  }

  grammar NTP {
    entry NTPHeader;
  }
}