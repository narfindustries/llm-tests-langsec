type Root {
    Packet packets[];
}

enum PacketType : u8 {
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

type Packet {
    PacketType type : u4;
    u4 flags;
    u32 remainingLength;
    switch (type) {
        case CONNECT: ConnectPacket connect;
        case CONNACK: ConnackPacket connack;
        case PUBLISH: PublishPacket publish;
        case PUBACK: PubackPacket puback;
        case PUBREC: PubrecPacket pubrec;
        case PUBREL: PubrelPacket pubrel;
        case PUBCOMP: PubcompPacket pubcomp;
        case SUBSCRIBE: SubscribePacket subscribe;
        case SUBACK: SubackPacket suback;
        case UNSUBSCRIBE: UnsubscribePacket unsubscribe;
        case UNSUBACK: UnsubackPacket unsuback;
        case PINGREQ: PingreqPacket pingreq;
        case PINGRESP: PingrespPacket pingresp;
        case DISCONNECT: DisconnectPacket disconnect;
        case AUTH: AuthPacket auth;
    }
}

type ConnectPacket {
    u8 protocolNameLen;
    u8 protocolName[protocolNameLen];
    u8 protocolLevel;
    ConnectFlags connectFlags;
    u16 keepAlive;
    Properties properties;
    u8 clientIdLen;
    u8 clientId[clientIdLen];
    optional {
        u8 willPropertiesLen;
        u8 willProperties[willPropertiesLen];
        u8 willTopicLen;
        u8 willTopic[willTopicLen];
        u8 willPayloadLen;
        u8 willPayload[willPayloadLen];
    } if (connectFlags.willFlag);
    optional {
        u8 usernameLen;
        u8 username[usernameLen];
    } if (connectFlags.usernameFlag);
    optional {
        u8 passwordLen;
        u8 password[passwordLen];
    } if (connectFlags.passwordFlag);
}

type ConnectFlags {
    u1 usernameFlag;
    u1 passwordFlag;
    u1 willRetain;
    u2 willQoS;
    u1 willFlag;
    u1 cleanStart;
    u1 reserved;
}

type Properties {
    u8 length;
    u8 properties[length];
}

type ConnackPacket {
    u8 connectAcknowledgeFlags;
    u8 reasonCode;
    Properties properties;
}

type PublishPacket {
    u8 topicNameLen;
    u8 topicName[topicNameLen];
    optional u16 packetIdentifier if (flags & 0x06);
    Properties properties;
    u8 payloadLen;
    u8 payload[payloadLen];
}

type PubackPacket {
    u16 packetIdentifier;
    u8 reasonCode;
    Properties properties;
}

type PubrecPacket {
    u16 packetIdentifier;
    u8 reasonCode;
    Properties properties;
}

type PubrelPacket {
    u16 packetIdentifier;
    u8 reasonCode;
    Properties properties;
}

type PubcompPacket {
    u16 packetIdentifier;
    u8 reasonCode;
    Properties properties;
}

type SubscribePacket {
    u16 packetIdentifier;
    Properties properties;
    Subscription subscriptions[];
}

type Subscription {
    u8 topicFilterLen;
    u8 topicFilter[topicFilterLen];
    SubscriptionOptions options;
}

type SubscriptionOptions {
    u2 qos;
    u1 noLocal;
    u1 retainAsPublished;
    u2 retainHandling;
    u2 reserved;
}

type SubackPacket {
    u16 packetIdentifier;
    Properties properties;
    u8 reasonCodes[];
}

type UnsubscribePacket {
    u16 packetIdentifier;
    Properties properties;
    TopicFilter topicFilters[];
}

type TopicFilter {
    u8 topicFilterLen;
    u8 topicFilter[topicFilterLen];
}

type UnsubackPacket {
    u16 packetIdentifier;
    Properties properties;
    u8 reasonCodes[];
}

type PingreqPacket {
    // No variable header or payload
}

type PingrespPacket {
    // No variable header or payload
}

type DisconnectPacket {
    u8 reasonCode;
    Properties properties;
}

type AuthPacket {
    u8 reasonCode;
    Properties properties;
}