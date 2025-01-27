@type("MQTT")
struct MqttPacket {
    header: MqttHeader,
    variable_header: VariableHeader,
    payload: Payload
}

@type("MQTTHeader")
struct MqttHeader {
    byte1: u8,
    remaining_length: varint
}

@type("VariableHeader")
@resolve("MqttPacket.header.byte1")
switch VariableHeader {
    case 0x10: ConnectHeader,
    case 0x20: ConnackHeader,
    case 0x30: PublishHeader,
    case 0x40: PubackHeader,
    case 0x50: PubrecHeader,
    case 0x60: PubrelHeader,
    case 0x70: PubcompHeader,
    case 0x80: SubscribeHeader,
    case 0x90: SubackHeader,
    case 0xA0: UnsubscribeHeader,
    case 0xB0: UnsubackHeader,
    case 0xC0: PingreqHeader,
    case 0xD0: PingrespHeader,
    case 0xE0: DisconnectHeader,
    case 0xF0: AuthHeader
}

@type("Payload")
@resolve("MqttPacket.header.byte1")
switch Payload {
    case 0x30: PublishPayload,
    case 0x80: SubscribePayload,
    case 0xA0: UnsubscribePayload,
    default: EmptyPayload
}

struct ConnectHeader {
    protocol_name_length: u16be,
    protocol_name: string(protocol_name_length),
    protocol_level: u8,
    connect_flags: u8,
    keep_alive: u16be
}

struct ConnackHeader {
    connection_acknowledge_flags: u8,
    connect_return_code: u8
}

struct PublishHeader {
    topic_name_length: u16be,
    topic_name: string(topic_name_length),
    packet_identifier: u16be
}

struct PubackHeader {
    packet_identifier: u16be
}

struct PubrecHeader {
    packet_identifier: u16be
}

struct PubrelHeader {
    packet_identifier: u16be
}

struct PubcompHeader {
    packet_identifier: u16be
}

struct SubscribeHeader {
    packet_identifier: u16be
}

struct SubackHeader {
    packet_identifier: u16be
}

struct UnsubscribeHeader {
    packet_identifier: u16be
}

struct UnsubackHeader {
    packet_identifier: u16be
}

struct PingreqHeader {}

struct PingrespHeader {}

struct DisconnectHeader {}

struct AuthHeader {}

struct PublishPayload {
    data: bytes
}

struct SubscribePayload {
    data: bytes
}

struct UnsubscribePayload {
    data: bytes
}

struct EmptyPayload {}

varint ::= {
    initial: u8,
    @repeat("initial & 0x80") continuation: u8
}