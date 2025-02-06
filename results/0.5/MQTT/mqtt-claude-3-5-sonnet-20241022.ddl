def PacketType = enum uint8 {
    RESERVED = 0,
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

def PropertyIdentifier = enum uint8 {
    PAYLOAD_FORMAT_INDICATOR = 0x01,
    MESSAGE_EXPIRY_INTERVAL = 0x02,
    CONTENT_TYPE = 0x03,
    RESPONSE_TOPIC = 0x08,
    CORRELATION_DATA = 0x09,
    SUBSCRIPTION_IDENTIFIER = 0x0B,
    SESSION_EXPIRY_INTERVAL = 0x11,
    ASSIGNED_CLIENT_IDENTIFIER = 0x12,
    SERVER_KEEP_ALIVE = 0x13,
    AUTHENTICATION_METHOD = 0x15,
    AUTHENTICATION_DATA = 0x16,
    REQUEST_PROBLEM_INFORMATION = 0x17,
    WILL_DELAY_INTERVAL = 0x18,
    REQUEST_RESPONSE_INFORMATION = 0x19,
    RESPONSE_INFORMATION = 0x1A,
    SERVER_REFERENCE = 0x1C,
    REASON_STRING = 0x1F,
    RECEIVE_MAXIMUM = 0x21,
    TOPIC_ALIAS_MAXIMUM = 0x22,
    TOPIC_ALIAS = 0x23,
    MAXIMUM_QOS = 0x24,
    RETAIN_AVAILABLE = 0x25,
    USER_PROPERTY = 0x26,
    MAXIMUM_PACKET_SIZE = 0x27,
    WILDCARD_SUBSCRIPTION_AVAILABLE = 0x28,
    SUBSCRIPTION_IDENTIFIER_AVAILABLE = 0x29,
    SHARED_SUBSCRIPTION_AVAILABLE = 0x2A
}

def VarByteInt = {
    byte1: uint8
    value: uint32 = byte1 & 0x7F
    if (byte1 & 0x80) != 0 {
        byte2: uint8
        value = value | ((byte2 & 0x7F) << 7)
        if (byte2 & 0x80) != 0 {
            byte3: uint8
            value = value | ((byte3 & 0x7F) << 14)
            if (byte3 & 0x80) != 0 {
                byte4: uint8
                value = value | ((byte4 & 0x7F) << 21)
            }
        }
    }
    value
}

def UTF8String = {
    length: uint16
    data: bytes(length)
}

def Property = {
    identifier: PropertyIdentifier
    value: select identifier {
        PAYLOAD_FORMAT_INDICATOR -> uint8
        MESSAGE_EXPIRY_INTERVAL -> uint32
        CONTENT_TYPE -> UTF8String
        RESPONSE_TOPIC -> UTF8String
        CORRELATION_DATA -> {
            length: uint16
            data: bytes(length)
        }
        SUBSCRIPTION_IDENTIFIER -> VarByteInt
        SESSION_EXPIRY_INTERVAL -> uint32
        ASSIGNED_CLIENT_IDENTIFIER -> UTF8String
        SERVER_KEEP_ALIVE -> uint16
        AUTHENTICATION_METHOD -> UTF8String
        AUTHENTICATION_DATA -> {
            length: uint16
            data: bytes(length)
        }
        REQUEST_PROBLEM_INFORMATION -> uint8
        WILL_DELAY_INTERVAL -> uint32
        REQUEST_RESPONSE_INFORMATION -> uint8
        RESPONSE_INFORMATION -> UTF8String
        SERVER_REFERENCE -> UTF8String
        REASON_STRING -> UTF8String
        RECEIVE_MAXIMUM -> uint16
        TOPIC_ALIAS_MAXIMUM -> uint16
        TOPIC_ALIAS -> uint16
        MAXIMUM_QOS -> uint8
        RETAIN_AVAILABLE -> uint8
        USER_PROPERTY -> {
            key: UTF8String
            value: UTF8String
        }
        MAXIMUM_PACKET_SIZE -> uint32
        WILDCARD_SUBSCRIPTION_AVAILABLE -> uint8
        SUBSCRIPTION_IDENTIFIER_AVAILABLE -> uint8
        SHARED_SUBSCRIPTION_AVAILABLE -> uint8
    }
}

def Properties = {
    length: VarByteInt
    properties: Property[while offset < length]
}

def FixedHeader = {
    type: PacketType : 4
    flags: uint4
    remainingLength: VarByteInt
}

def ConnectFlags = bitfield uint8 {
    reserved: 1
    cleanStart: 1
    willFlag: 1
    willQoS: 2
    willRetain: 1
    passwordFlag: 1
    usernameFlag: 1
}

def Connect = {
    protocolName: UTF8String
    protocolVersion: uint8
    connectFlags: ConnectFlags
    keepAlive: uint16
    properties: Properties
    clientId: UTF8String
    if connectFlags.willFlag == 1 {
        willProperties: Properties
        willTopic: UTF8String
        willPayload: {
            length: uint16
            data: bytes(length)
        }
    }
    if connectFlags.usernameFlag == 1 {
        username: UTF8String
    }
    if connectFlags.passwordFlag == 1 {
        password: UTF8String
    }
}

def ConnAckFlags = bitfield uint8 {
    reserved: 7
    sessionPresent: 1
}

def ConnAck = {
    flags: ConnAckFlags
    reasonCode: uint8
    properties: Properties
}

def Publish = {
    topicName: UTF8String
    if header.flags.qos > 0 {
        packetId: uint16
    }
    properties: Properties
    payload: bytes(remainingLength - offset)
}

def MQTTPacket = {
    header: FixedHeader
    body: select header.type {
        CONNECT -> Connect
        CONNACK -> ConnAck
        PUBLISH -> Publish
        PUBACK -> {
            packetId: uint16
            reasonCode: uint8
            properties: Properties
        }
        PUBREC -> {
            packetId: uint16
            reasonCode: uint8
            properties: Properties
        }
        PUBREL -> {
            packetId: uint16
            reasonCode: uint8
            properties: Properties
        }
        PUBCOMP -> {
            packetId: uint16
            reasonCode: uint8
            properties: Properties
        }
        SUBSCRIBE -> {
            packetId: uint16
            properties: Properties
            subscriptions: {
                topicFilter: UTF8String
                options: uint8
            }[while offset < remainingLength]
        }
        SUBACK -> {
            packetId: uint16
            properties: Properties
            returnCodes: uint8[while offset < remainingLength]
        }
        UNSUBSCRIBE -> {
            packetId: uint16
            properties: Properties
            topicFilters: UTF8String[while offset < remainingLength]
        }
        UNSUBACK -> {
            packetId: uint16
            properties: Properties
            reasonCodes: uint8[while offset < remainingLength]
        }
        PINGREQ -> void
        PINGRESP -> void
        DISCONNECT -> {
            reasonCode: uint8
            properties: Properties
        }
        AUTH -> {
            reasonCode: uint8
            properties: Properties
        }
    }
}

entrypoint MQTTPacket