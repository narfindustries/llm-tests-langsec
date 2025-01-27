type Bit = U1
type Byte = U8
type Word16 = U16
type Word32 = U32

enum PacketType {
    CONNECT     = 1,
    CONNACK     = 2,
    PUBLISH     = 3,
    PUBACK      = 4,
    PUBREC      = 5,
    PUBREL      = 6,
    PUBCOMP     = 7,
    SUBSCRIBE   = 8,
    SUBACK      = 9,
    UNSUBSCRIBE = 10,
    UNSUBACK    = 11,
    PINGREQ     = 12,
    PINGRESP    = 13,
    DISCONNECT  = 14
}

struct FixedHeader {
    packet_type: PacketType,
    dup_flag: Bit,
    qos_level: U2,
    retain_flag: Bit,
    remaining_length: VarInt
}

type VarInt = {
    value: U32,
    encoded: [Byte]
}

struct ConnectPacket {
    protocol_name: String,
    protocol_level: Byte,
    connect_flags: Byte,
    keep_alive: Word16,
    client_id: String,
    will_topic: optional String,
    will_message: optional String,
    username: optional String,
    password: optional String
}

struct ConnackPacket {
    session_present: Bit,
    return_code: Byte
}

struct PublishPacket {
    topic_name: String,
    packet_id: optional Word16,
    payload: [Byte]
}

struct SubscribePacket {
    packet_id: Word16,
    topics: [{
        topic_filter: String,
        max_qos: U2
    }]
}

struct SubackPacket {
    packet_id: Word16,
    return_codes: [Byte]
}

struct UnsubscribePacket {
    packet_id: Word16,
    topics: [String]
}

struct PingReqPacket {}
struct PingRespPacket {}
struct DisconnectPacket {}

fn encode_variable_int(value: U32) -> [Byte] {
    let mut encoded: [Byte] = [];
    let mut x = value;
    
    while x > 0 {
        let encoded_byte = x % 128;
        x = x / 128;
        
        if x > 0 {
            encoded.push(encoded_byte | 0x80);
        } else {
            encoded.push(encoded_byte);
        }
    }
    
    return encoded;
}

fn decode_variable_int(encoded: [Byte]) -> VarInt {
    let mut value: U32 = 0;
    let mut multiplier: U32 = 1;
    let mut index = 0;
    
    while index < encoded.len() {
        let current_byte = encoded[index];
        value += (current_byte & 0x7F) * multiplier;
        multiplier *= 128;
        
        if (current_byte & 0x80) == 0 {
            break;
        }
        
        index += 1;
    }
    
    return VarInt { 
        value: value, 
        encoded: encoded[0..index+1] 
    };
}