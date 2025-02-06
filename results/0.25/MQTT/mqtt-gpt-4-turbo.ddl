module MQTT;

import std.core;

// MQTT Control Packet Types
enum PacketType : uint4 {
    CONNECT = 1,
    CONNACK = 2,
    PUBLISH = 3,
    PUBACK = 4,
    PUBREC = 5,
    PUBREL = 6,
    PUBCOMP = 7,
    SUBSCRIBE = 8,
    SUBACK = 9,
    UNSUBSCRIBE = 10,
    UNSUBACK = 11,
    PINGREQ = 12,
    PINGRESP = 13,
    DISCONNECT = 14,
    AUTH = 15
}

// Fixed Header for all MQTT packets
struct FixedHeader {
    packetType : PacketType;
    flags : uint4;
    remainingLength : VarInt;
}

// Variable Byte Integer as per MQTT specification
struct VarInt {
    value : uint32;
}

// MQTT CONNECT Packet
struct ConnectPacket {
    header : FixedHeader;
    protocolName : string;
    protocolLevel : uint8;
    connectFlags : uint8;
    keepAlive : uint16;
    properties : Properties;
    clientIdentifier : string;
    willProperties : optional<Properties>;
    willTopic : optional<string>;
    willPayload : optional<bytes>;
    username : optional<string>;
    password : optional<bytes>;
}

// MQTT CONNACK Packet
struct ConnackPacket {
    header : FixedHeader;
    connectAcknowledgeFlags : uint8;
    connectReasonCode : uint8;
    properties : Properties;
}

// MQTT PUBLISH Packet
struct PublishPacket {
    header : FixedHeader;
    topicName : string;
    packetIdentifier : optional<uint16>;
    properties : Properties;
    payload : bytes;
}

// MQTT SUBSCRIBE Packet
struct SubscribePacket {
    header : FixedHeader;
    packetIdentifier : uint16;
    properties : Properties;
    subscriptionData : array<SubscriptionData>;
}

struct SubscriptionData {
    topicFilter : string;
    options : uint8;
}

// MQTT SUBACK Packet
struct SubackPacket {
    header : FixedHeader;
    packetIdentifier : uint16;
    properties : Properties;
    returnCodes : array<uint8>;
}

// MQTT UNSUBSCRIBE Packet
struct UnsubscribePacket {
    header : FixedHeader;
    packetIdentifier : uint16;
    properties : Properties;
    topicFilters : array<string>;
}

// MQTT UNSUBACK Packet
struct UnsubackPacket {
    header : FixedHeader;
    packetIdentifier : uint16;
    properties : Properties;
    reasonCodes : array<uint8>;
}

// MQTT PINGREQ, PINGRESP Packets
struct PingPacket {
    header : FixedHeader;
}

// MQTT DISCONNECT, AUTH Packets
struct DisconnectAuthPacket {
    header : FixedHeader;
    reasonCode : uint8;
    properties : Properties;
}

// MQTT Properties (common structure for all properties in MQTT packets)
struct Properties {
    length : VarInt;
    value : bytes; // This should be parsed according to the property identifier and length
}

// Main MQTT Packet structure
struct MQTTPacket {
    header : FixedHeader;
    payload : switch (header.packetType) {
        case PacketType.CONNECT: ConnectPacket;
        case PacketType.CONNACK: ConnackPacket;
        case PacketType.PUBLISH: PublishPacket;
        case PacketType.SUBSCRIBE: SubscribePacket;
        case PacketType.SUBACK: SubackPacket;
        case PacketType.UNSUBSCRIBE: UnsubscribePacket;
        case PacketType.UNSUBACK: UnsubackPacket;
        case PacketType.PINGREQ, PacketType.PINGRESP: PingPacket;
        case PacketType.DISCONNECT, PacketType.AUTH: DisconnectAuthPacket;
        default: bytes;
    };
}