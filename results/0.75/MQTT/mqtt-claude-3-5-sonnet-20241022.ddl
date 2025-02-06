def Main = {
    VarByteInt = {
        x : uint8[];
        x.length <= 4;
        match x.length {
            1 => x[0] < 128,
            2 => x[0] >= 128 && x[1] < 128,
            3 => x[0] >= 128 && x[1] >= 128 && x[2] < 128,
            4 => x[0] >= 128 && x[1] >= 128 && x[2] >= 128 && x[3] < 128
        }
    }

    UTF8String = {
        length: uint16;
        data: uint8[length]
    }

    PacketType = {
        type: uint4;
        type >= 1 && type <= 15
    }

    Flags = {
        dup: uint1;
        qos: uint2;
        retain: uint1
    }

    FixedHeader = {
        packetType: PacketType;
        flags: Flags;
        remainingLength: VarByteInt
    }

    Property = {
        identifier: uint8;
        value: select identifier {
            0x01 => uint8,
            0x02 => uint32,
            0x03 => UTF8String,
            0x08 => UTF8String,
            0x09 => uint8[],
            0x0B => VarByteInt,
            0x11 => uint32,
            0x15 => UTF8String,
            0x16 => uint8[],
            0x17 => uint8,
            0x19 => uint8,
            0x21 => uint16,
            0x22 => uint16,
            0x23 => uint16,
            0x26 => {
                key: UTF8String;
                value: UTF8String
            },
            0x27 => uint32,
            0x1C => UTF8String,
            0x1F => UTF8String
        }
    }

    Properties = {
        length: VarByteInt;
        properties: Property[while !EOF]
    }

    ConnectFlags = {
        reserved: uint1 = 0;
        cleanStart: uint1;
        willFlag: uint1;
        willQoS: uint2;
        willRetain: uint1;
        passwordFlag: uint1;
        usernameFlag: uint1
    }

    Connect = {
        protocolName: UTF8String;
        protocolVersion: uint8 = 5;
        connectFlags: ConnectFlags;
        keepAlive: uint16;
        properties: Properties;
        clientId: UTF8String;
        willProperties: if connectFlags.willFlag then Properties else void;
        willTopic: if connectFlags.willFlag then UTF8String else void;
        willPayload: if connectFlags.willFlag then uint8[] else void;
        username: if connectFlags.usernameFlag then UTF8String else void;
        password: if connectFlags.passwordFlag then UTF8String else void
    }

    ConnAck = {
        flags: uint8;
        reasonCode: uint8;
        properties: Properties
    }

    Publish = {
        topicName: UTF8String;
        packetId: if header.flags.qos > 0 then uint16 else void;
        properties: Properties;
        payload: uint8[]
    }

    PubAck = {
        packetId: uint16;
        reasonCode: uint8;
        properties: Properties
    }

    Subscribe = {
        packetId: uint16;
        properties: Properties;
        subscriptions: {
            topicFilter: UTF8String;
            options: uint8
        }[]
    }

    SubAck = {
        packetId: uint16;
        properties: Properties;
        reasonCodes: uint8[]
    }

    Unsubscribe = {
        packetId: uint16;
        properties: Properties;
        topicFilters: UTF8String[]
    }

    UnsubAck = {
        packetId: uint16;
        properties: Properties;
        reasonCodes: uint8[]
    }

    Disconnect = {
        reasonCode: uint8;
        properties: Properties
    }

    Auth = {
        reasonCode: uint8;
        properties: Properties
    }

    MQTTPacket = {
        header: FixedHeader;
        body: select header.packetType.type {
            1 => Connect,
            2 => ConnAck,
            3 => Publish,
            4 => PubAck,
            5 => PubAck,
            6 => PubAck,
            7 => PubAck,
            8 => Subscribe,
            9 => SubAck,
            10 => Unsubscribe,
            11 => UnsubAck,
            12 => void,
            13 => void,
            14 => Disconnect,
            15 => Auth
        }
    }

    MQTTPacket
}