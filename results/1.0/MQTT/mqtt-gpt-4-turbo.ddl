module MQTT;

import Dae;

type Byte = U8;

alias VarInt = Dae.VarInt;
alias Binary = List[U8];

type UTF8String = Dae.Field(
  len: U16,
  data: Binary(len)
);

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
};

struct FixedHeader {
  packetType: PacketType;
  flags: U8;
  remainingLength: VarInt;
}

struct ProtocolNameAndVersion {
  name: UTF8String;
  version: U8;
}

struct ConnectFlags {
  reserved: U1 = 0;
  cleanStart: U1;
  willFlag: U1;
  willQoS: U2;
  willRetain: U1;
  passwordFlag: U1;
  usernameFlag: U1;
}

struct ConnectVariableHeader {
  protocolNameAndVersion: ProtocolNameAndVersion;
  connectFlags: ConnectFlags;
  keepAlive: U16;
  properties: Properties;
}

type PacketIdentifier = U16;

typedef Properties = Dae.Map[UTF8String, UTF8String];

struct PublishVariableHeader {
  topicName: UTF8String;
  packetIdentifier: PacketIdentifier;
  properties: Properties;
}

struct ConnectPayload {
  clientIdentifier: UTF8String;
  willProperties: Properties;
  willTopic: UTF8String;
  willMessage: Binary;
  userName: UTF8String;
  password: Binary;
}

struct SubscribePayload {
  packetIdentifier: PacketIdentifier;
  properties: Properties;
  topicFilters: List[Subscription];
}

struct Subscription {
  topicFilter: UTF8String;
  options: U8;
}

struct UnsubscribePayload {
  packetIdentifier: PacketIdentifier;
  properties: Properties;
  topicFilters: List[UTF8String];
}

union Payload depending on (PacketType) {
  case CONNECT:
    ConnectPayload;
  case PUBLISH:
    Binary;
  case SUBSCRIBE:
    SubscribePayload;
  case UNSUBSCRIBE:
    UnsubscribePayload;
  default:
    Binary;
}

struct MQTTMessage {
  header: FixedHeader;
  variableHeader: Binary depending on (header.packetType);
  payload: Payload depending on (header.packetType);
}

