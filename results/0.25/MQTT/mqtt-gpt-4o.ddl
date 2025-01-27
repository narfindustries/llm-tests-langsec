type MQTT = struct {
    fixed_header: FixedHeader,
    variable_header: VariableHeader,
    payload: Payload
}

type FixedHeader = struct {
    byte1: u8,
    remaining_length: VLInt
}

type VariableHeader = switch (fixed_header.byte1 & 0xF0) {
    0x10 => ConnectHeader,
    0x20 => ConnAckHeader,
    0x30 => PublishHeader,
    0x40 => PubAckHeader,
    0x50 => PubRecHeader,
    0x60 => PubRelHeader,
    0x70 => PubCompHeader,
    0x80 => SubscribeHeader,
    0x90 => SubAckHeader,
    0xA0 => UnsubscribeHeader,
    0xB0 => UnsubAckHeader,
    0xC0 => PingReqHeader,
    0xD0 => PingRespHeader,
    0xE0 => DisconnectHeader,
    0xF0 => AuthHeader
}

type Payload = switch (fixed_header.byte1 & 0xF0) {
    0x30 => PublishPayload,
    0x80 => SubscribePayload,
    0xA0 => UnsubscribePayload,
    _ => EmptyPayload
}

type VLInt = vlq

type ConnectHeader = struct {
    protocol_name: String,
    protocol_level: u8,
    connect_flags: u8,
    keep_alive: u16
}

type ConnAckHeader = struct {
    connect_ack_flags: u8,
    connect_return_code: u8
}

type PublishHeader = struct {
    topic_name: String,
    packet_identifier: u16
}

type PubAckHeader = struct {
    packet_identifier: u16
}

type PubRecHeader = struct {
    packet_identifier: u16
}

type PubRelHeader = struct {
    packet_identifier: u16
}

type PubCompHeader = struct {
    packet_identifier: u16
}

type SubscribeHeader = struct {
    packet_identifier: u16
}

type SubAckHeader = struct {
    packet_identifier: u16
}

type UnsubscribeHeader = struct {
    packet_identifier: u16
}

type UnsubAckHeader = struct {
    packet_identifier: u16
}

type PingReqHeader = struct {}

type PingRespHeader = struct {}

type DisconnectHeader = struct {}

type AuthHeader = struct {}

type PublishPayload = struct {
    message: Bytes
}

type SubscribePayload = struct {
    topics: List<String>
}

type UnsubscribePayload = struct {
    topics: List<String>
}

type EmptyPayload = struct {}

type String = prefixed_string(u16)

type Bytes = list[u8]