grammar MQTT;

import DAEDALUS::BitManip;
import DAEDALUS::Bytes;

enum PacketType {
  CONNECT = 1,
  CONNACK,
  PUBLISH,
  PUBACK,
  PUBREC,
  PUBREL,
  PUBCOMP,
  SUBSCRIBE,
  SUBACK,
  UNSUBSCRIBE,
  UNSUBACK,
  PINGREQ,
  PINGRESP,
  DISCONNECT,
  AUTH
}

struct MQTT_Packet {
  u4 packetType : PacketType;
  u4 flags;
  VarInt remainingLength;
  choice packet : packetType {
    CONNECT => ConnectPacket,
    CONNACK => ConnackPacket,
    PUBLISH => PublishPacket,
    SUBSCRIBE => SubscribePacket,
    SUBACK => SubackPacket,
    UNSUBSCRIBE => UnsubscribePacket,
    UNSUBACK => UnsubackPacket,
    PINGREQ => EmptyPayload,
    PINGRESP => EmptyPayload,
    DISCONNECT => DisconnectPacket,
    PUBACK => AckPacket,
    PUBREC => AckPacket,
    PUBREL => AckPacket,
    PUBCOMP => AckPacket,
    AUTH => AuthPacket
  };
}

struct VarInt {
  bytes data;
  u32 : value = BitManip::VarInt(data);
}

struct ConnectPacket {
  UTF8String protocolName;
  u8 protocolLevel;
  u8 connectFlags;
  u16 keepAlive;
  Properties properties;
  UTF8String clientIdentifier;
  optional<WillMessage> willMessage : (connectFlags & 0x04) != 0;
  optional<UTF8String> userName : (connectFlags & 0x80) != 0;
  optional<Bytes> password : (connectFlags & 0x40) != 0;
}

struct ConnackPacket {
  u8 acknowledgeFlags;
  u8 reasonCode;
  Properties properties;
}

struct PublishPacket {
  optional<u16> packetIdentifier : (BitManip::Extract(flags, 1, 2) != 0);
  Properties properties;
  UTF8String topicName;
  Bytes payload;
}

struct SubscribePacket {
  u16 packetIdentifier;
  Properties properties;
  array<Subscription> subscriptions;
}

struct Subscription {
  UTF8String topicFilter;
  u8 options;
}

struct SubackPacket {
  u16 packetIdentifier;
  Properties properties;
  array<u8> reasonCodes;
}

struct UnsubscribePacket {
  u16 packetIdentifier;
  Properties properties;
  array<UTF8String> topics;
}

struct UnsubackPacket {
  u16 packetIdentifier;
  Properties properties;
  array<u8> reasonCodes;
}

struct DisconnectPacket {
  u8 reasonCode;
  Properties properties;
}

struct AckPacket {
  u16 packetIdentifier;
  u8 reasonCode;
  Properties properties;
}

struct AuthPacket {
  u8 reasonCode;
  Properties properties;
}

struct EmptyPayload {}

struct Properties {
  array<Property> properties;
}

struct Property {
  u8 propertyIdentifier;
  choice propertyValue : propertyIdentifier {
    0x01 => u32, // Session Expiry Interval
    0x02 => UTF8String, // Authentication Method
    // Add other properties as needed
  };
}

struct UTF8String {
  VarInt length;
  string value : length;
}

struct WillMessage {
  Properties properties;
  UTF8String topic;
  Bytes message;
}