domain NetworkTimeProtocolVersion4 {
  uint8 mode;
  uint8 poll;
  uint8 precision;
  uint8 delay;
  uint32 dispersion;
  uint32 identifier;
  uint32 referenceTimestampSeconds;
  uint32 referenceTimestampFraction;
  uint32 originateTimestampSeconds;
  uint32 originateTimestampFraction;
  uint32 receiveTimestampSeconds;
  uint32 receiveTimestampFraction;
  uint32 transmitTimestampSeconds;
  uint32 transmitTimestampFraction;

  constraint {
    mode == 0x24;
  }

  byte[] extensions;
}

instance {
  virtual bool parse(NetworkTimeProtocolVersion4& ntp) {
    ntp.mode = next(1);
    ntp.poll = next(1);
    ntp.precision = next(1);
    ntp.delay = next(1);
    ntp.dispersion = next(4);
    ntp.identifier = next(4);
    ntp.referenceTimestampSeconds = next(4);
    ntp.referenceTimestampFraction = next(4);
    ntp.originateTimestampSeconds = next(4);
    ntp.originateTimestampFraction = next(4);
    ntp.receiveTimestampSeconds = next(4);
    ntp.receiveTimestampFraction = next(4);
    ntp.transmitTimestampSeconds = next(4);
    ntp.transmitTimestampFraction = next(4);

    while (hasNext()) {
      // Consume extensions data
      byte[] extData = nextBytes(4);
      if (extData[0] != 0x00) {
        // Handle extension field
        uint16 fieldType = (uint16) ((extData[1] << 8) | extData[2]);
        uint16 fieldLength = (uint16) ((extData[3] << 8) | next(1));
        skip(fieldLength - 4); // Skips the extension field
      } else {
        break;
      }
    }
    return true;
  }

  bool hasNext() {
    return pos < length;
  }

  uint8 next(uint bits) {
    if (bits == 8) {
      return (uint8) nextByte();
    } else {
      throw "Unsupported bit size for primitive data type";
    }
  }

  uint16 nextShort() {
    return (uint16) ((nextByte() << 8) | nextByte());
  }

  uint32 nextInt() {
    return (uint32) ((nextByte() << 24) | (nextByte() << 16) | (nextByte() << 8) | nextByte());
  }

  uint64 nextLong() {
    return (uint64) ((nextByte() << 56) | (nextByte() << 48) | (nextByte() << 40) | (nextByte() << 32) | (nextByte() << 24) | (nextByte() << 16) | (nextByte() << 8) | nextByte());
  }

  byte nextByte() {
    if (pos < length) {
      return data[pos++];
    }
    throw "Unexpected end of data";
  }

  byte[] nextBytes(uint length) {
    if (pos + length <= this.length) {
      byte[] result = new byte[length];
      for (int i = 0; i < length; i++) {
        result[i] = data[pos++];
      }
      return result;
    }
    throw "Not enough data";
  }

  void skip(uint length) {
    pos += length;
  }

  uint pos = 0;
  byte[] data;
  uint length;
}