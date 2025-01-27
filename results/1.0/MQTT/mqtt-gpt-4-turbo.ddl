module MQTT {

  import BE;
  import Util;

  type Byte = BE.u8;
  type Word = BE.u16;
  type DWord = BE.u32;

  type ByteArray(length: Exp<Size>) = [Byte]#length;

  type Nibble = Bits(4);

  @[Size=8]
  union Packet {
    struct {
      header: FixedHeader;
      payload: switch header.controlPacketType {
        case 1  => ConnectPacket;
        case 2  => ConnAckPacket;
        case 3  => PublishPacket(header);
        case 8  => SubscribePacket;
        case 9  => SubAckPacket;
        case 10 => UnsubscribePacket;
        case 12 => PingReqPacket;
        case 13 => PingRespPacket;
        case 14 => DisconnectPacket;
      };
    };
  }

  @[Size=8]
  struct FixedHeader {
    controlPacketType: Nibble;
    flags: Nibble;
    remainingLength: VariableLength;
  }

  alias VariableLength = VLInt;

  @[Size=8]
  struct VLInt {
    value: Exp<Size> = BE.u7;
    hasMore: Bit;
    next: switch hasMore {
      case 1 => VLInt;
      case 0 => EmptyType;
    };
  }

  type ConnectPacket = struct {
    protocolName: LengthPrefixedString;
    versionNumber: Byte;
    connectFlags: Byte;
    keepAlive: Word;
    clientIdentifier: LengthPrefixedString;
    willTopic: switch ((connectFlags >> 2) & 0x01) {
      case 1 => LengthPrefixedString;
      default => EmptyType;
    };
    willMessage: switch ((connectFlags >> 2) & 0x01) {
      case 1 => LengthPrefixedString;
      default => EmptyType;
    };
    userName: switch (connectFlags >> 7) {
      case 1 => LengthPrefixedString;
      default => EmptyType;
    };
    password: switch ((connectFlags >> 6) & 0x01) {
      case 1 => LengthPrefixedString;
      default => EmptyType;
    };
  }

  type ConnAckPacket = struct {
    reserved: Byte#1;
    returnCode: Byte;
  }

  type PublishPacket(header: FixedHeader) = struct {
    topicName: LengthPrefixedString;
    packetIdentifier: switch ((header.flags >> 1) & 0x03) {
      case 0 => EmptyType;
      case 1 => Word;
      case 2 => Word;
      case 3 => Word;
      default => EmptyType;
    };
    payload: ByteArray(header.remainingLength);
  }

  type SubscribePacket = struct {
    packetIdentifier: Word;
    topicFilters: [SubscriptionRequest]#_;
  }

  type SubAckPacket = struct {
    packetIdentifier: Word;
    returnCodes: [Byte]#_;
  }

  type UnsubscribePacket = struct {
    packetIdentifier: Word;
    topicFilters: [LengthPrefixedString]#_;
  }

  type PingReqPacket = EmptyType;
  type PingRespPacket = EmptyType;
  type DisconnectPacket = EmptyType;

  type SubscriptionRequest = struct {
    topicFilter: LengthPrefixedString;
    requestedQoS: Byte;
  }

  type LengthPrefixedString = struct {
    length: Word;
    string: ByteArray(length);
  }

  type EmptyType = struct { }
}