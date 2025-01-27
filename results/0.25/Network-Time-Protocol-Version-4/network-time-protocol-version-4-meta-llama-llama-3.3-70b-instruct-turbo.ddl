domain NetworkTimeProtocolVersion4 {
  types {
    uint8  : 1 byte;
    uint16 : 2 bytes;
    uint32 : 4 bytes;
    uint64 : 8 bytes;
  }

  type NTPPacket {
    leap              : uint8;
    version           : uint8;
    mode              : uint8;
    stratum           : uint8;
    poll              : uint8;
    precision         : uint8;
    delay             : uint32;
    dispersion        : uint32;
    identifier        : uint32;
    referenceTS       : uint64;
    originateTS       : uint64;
    receiveTS         : uint64;
    transmitTS        : uint64;
    extensionField    : ExtensionField[];
  }

  type ExtensionField {
    type              : uint16;
    length            : uint16;
    value             : bytes(length * 4);
  }

  syntax NTPPacket {
    leap              : uint8   @ bits 6..7;
    version           : uint8   @ bits 3..5;
    mode              : uint8   @ bits 0..2;
    stratum           : uint8;
    poll              : uint8;
    precision         : uint8;
    delay             : uint32;
    dispersion        : uint32;
    identifier        : uint32;
    referenceTS       : uint64;
    originateTS       : uint64;
    receiveTS         : uint64;
    transmitTS        : uint64;
    extensionField    : ExtensionField[];
  }

  syntax ExtensionField {
    type              : uint16;
    length            : uint16;
    value             : bytes(length * 4);
  }
}