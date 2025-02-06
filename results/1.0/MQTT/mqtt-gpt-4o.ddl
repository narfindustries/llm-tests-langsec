enum ControlPacketType : uint8 {
    CONNECT = 1;
    CONNACK = 2;
    PUBLISH = 3;
    PUBACK = 4;
    PUBREC = 5;
    PUBREL = 6;
    PUBCOMP = 7;
    SUBSCRIBE = 8;
    SUBACK = 9;
    UNSUBSCRIBE = 10;
    UNSUBACK = 11;
    PINGREQ = 12;
    PINGRESP = 13;
    DISCONNECT = 14;
    AUTH = 15;
}

enum QoSLevel : uint2 {
    AT_MOST_ONCE = 0;
    AT_LEAST_ONCE = 1;
    EXACTLY_ONCE = 2;
}

struct Property {
    uint8 identifier;
    uint16 length;
    bytes value[length];
}

struct ConnectFlags {
    uint1 usernameFlag;
    uint1 passwordFlag;
    uint1 willRetain;
    QoSLevel willQoS;
    uint1 willFlag;
    uint1 cleanStart;
    uint1 reserved;
}

struct MQTTConnect {
    string protocolName as utf8(length=4);
    uint8 protocolLevel;
    ConnectFlags connectFlags;
    uint16 keepAlive;
    Property properties[];
    string clientIdentifier as utf8;
    Property willProperties[] if connectFlags.willFlag == 1;
    string willTopic as utf8 if connectFlags.willFlag == 1;
    bytes willMessage[] if connectFlags.willFlag == 1;
    string username as utf8 if connectFlags.usernameFlag == 1;
    bytes password[] if connectFlags.passwordFlag == 1;
}

struct CONNACKFlags {
    uint7 reserved;
    uint1 sessionPresent;
}

struct MQTTConnack {
    CONNACKFlags flags;
    uint8 reasonCode;
    Property properties[];
}

struct MQTTPublish {
    uint1 dup;
    QoSLevel qos;
    uint1 retain;
    uint5 reserved;
    string topicName as utf8;
    uint16 packetIdentifier if qos != QoSLevel.AT_MOST_ONCE;
    Property properties[];
    bytes payload[];
}

struct MQTTPuback {
    uint16 packetIdentifier;
    uint8 reasonCode;
    Property properties[];
}

struct MQTTPubrec {
    uint16 packetIdentifier;
    uint8 reasonCode;
    Property properties[];
}

struct MQTTPubrel {
    uint16 packetIdentifier;
    uint8 reasonCode;
    Property properties[];
}

struct MQTTPubcomp {
    uint16 packetIdentifier;
    uint8 reasonCode;
    Property properties[];
}

struct SubscriptionOptions {
    QoSLevel qos;
    uint1 noLocal;
    uint1 retainAsPublished;
    uint2 retainHandling;
}

struct MQTTSubscribe {
    uint16 packetIdentifier;
    Property properties[];
    struct {
        string topicFilter as utf8;
        SubscriptionOptions options;
    } subscriptions[];
}

struct MQTTSuback {
    uint16 packetIdentifier;
    Property properties[];
    uint8 reasonCodes[];
}

struct MQTTUnsubscribe {
    uint16 packetIdentifier;
    Property properties[];
    string topicFilters[] as utf8;
}

struct MQTTUnsuback {
    uint16 packetIdentifier;
    Property properties[];
    uint8 reasonCodes[];
}

struct MQTTDisconnect {
    uint8 reasonCode;
    Property properties[];
}

struct MQTTAuth {
    uint8 reasonCode;
    Property properties[];
}

struct MQTTPacket {
    ControlPacketType type;
    uint16 remainingLength;
    switch (type) {
        case ControlPacketType.CONNECT: MQTTConnect connect;
        case ControlPacketType.CONNACK: MQTTConnack connack;
        case ControlPacketType.PUBLISH: MQTTPublish publish;
        case ControlPacketType.PUBACK: MQTTPuback puback;
        case ControlPacketType.PUBREC: MQTTPubrec pubrec;
        case ControlPacketType.PUBREL: MQTTPubrel pubrel;
        case ControlPacketType.PUBCOMP: MQTTPubcomp pubcomp;
        case ControlPacketType.SUBSCRIBE: MQTTSubscribe subscribe;
        case ControlPacketType.SUBACK: MQTTSuback suback;
        case ControlPacketType.UNSUBSCRIBE: MQTTUnsubscribe unsubscribe;
        case ControlPacketType.UNSUBACK: MQTTUnsuback unsuback;
        case ControlPacketType.DISCONNECT: MQTTDisconnect disconnect;
        case ControlPacketType.AUTH: MQTTAuth auth;
        case ControlPacketType.PINGREQ: ; // No payload
        case ControlPacketType.PINGRESP: ; // No payload
    }
}