domain MQTT {
  const MAX_PACKET_SIZE = 268435455;
  const PROTOCOL_NAME = "MQTT";
  const PROTOCOL_VERSION = 5;

  type Byte = uint8;
  type UInt16 = uint16;
  type UInt32 = uint32;
  type VariableInt = uint32;

  syntax Packet {
    length: VariableInt;
    header: Byte;
    body: byte[length - 1];
  }

  syntax ConnectPacket {
    header: Byte = 0x10;
    protocolNameLength: UInt16;
    protocolName: byte[protocolNameLength];
    protocolVersion: Byte;
    connectFlags: Byte;
    keepAlive: UInt16;
    clientIdLength: UInt16;
    clientId: byte[clientIdLength];
  }

  syntax ConnAckPacket {
    header: Byte = 0x20;
    connectAckFlags: Byte;
    connectReasonCode: Byte;
  }

  syntax PublishPacket {
    header: Byte;
    topicNameLength: UInt16;
    topicName: byte[topicNameLength];
    packetId: UInt16;
    payload: byte[length - 2 - topicNameLength];
  }

  syntax PubAckPacket {
    header: Byte = 0x40;
    packetId: UInt16;
  }

  syntax PubRecPacket {
    header: Byte = 0x50;
    packetId: UInt16;
  }

  syntax PubRelPacket {
    header: Byte = 0x60;
    packetId: UInt16;
  }

  syntax PubCompPacket {
    header: Byte = 0x70;
    packetId: UInt16;
  }

  syntax SubscribePacket {
    header: Byte = 0x80;
    packetId: UInt16;
    subscriptions: byte[length - 2];
  }

  syntax SubAckPacket {
    header: Byte = 0x90;
    packetId: UInt16;
    returnCodes: byte[length - 2];
  }

  syntax UnsubscribePacket {
    header: Byte = 0xA0;
    packetId: UInt16;
    unsubscriptions: byte[length - 2];
  }

  syntax UnsubAckPacket {
    header: Byte = 0xB0;
    packetId: UInt16;
  }

  syntax PingReqPacket {
    header: Byte = 0xC0;
  }

  syntax PingRespPacket {
    header: Byte = 0xD0;
  }

  syntax DisconnectPacket {
    header: Byte = 0xE0;
    reasonCode: Byte;
   _MUX: [
      (
        reasonCode == 0x00 ||
        reasonCode == 0x04 ||
        reasonCode == 0x10 ||
        reasonCode == 0x14 ||
        reasonCode == 0x1F ||
        reasonCode == 0x7E ||
        reasonCode == 0x7F ||
        reasonCode == 0x80 ||
        reasonCode == 0xFF
      ): byte[0],
      (
        reasonCode == 0x01 ||
        reasonCode == 0x02 ||
        reasonCode == 0x03 ||
        reasonCode == 0x05 ||
        reasonCode == 0x11 ||
        reasonCode == 0x12 ||
        reasonCode == 0x13 ||
        reasonCode == 0x15 ||
        reasonCode == 0x16 ||
        reasonCode == 0x17 ||
        reasonCode == 0x18 ||
        reasonCode == 0x19 ||
        reasonCode == 0x1A ||
        reasonCode == 0x1B ||
        reasonCode == 0x1C ||
        reasonCode == 0x1D ||
        reasonCode == 0x1E ||
        reasonCode == 0x7D ||
        reasonCode == 0x7C
      ): byte[1]
    ];
  }
}