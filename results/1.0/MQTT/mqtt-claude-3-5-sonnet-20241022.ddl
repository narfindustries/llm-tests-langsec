def MQTT_VarLen = {
    let b1 = byte;
    if ((b1 & 0x80u) == 0u) {
        b1
    } else {
        let b2 = byte;
        if ((b2 & 0x80u) == 0u) {
            ((b1 & 0x7fu) | (b2 << 7u))
        } else {
            let b3 = byte;
            if ((b3 & 0x80u) == 0u) {
                ((b1 & 0x7fu) | ((b2 & 0x7fu) << 7u) | (b3 << 14u))
            } else {
                let b4 = byte;
                ((b1 & 0x7fu) | ((b2 & 0x7fu) << 7u) | ((b3 & 0x7fu) << 14u) | ((b4 & 0x7fu) << 21u))
            }
        }
    }
}

def MQTT_String = {
    let len = uint16;
    bytes(len)
}

def MQTT_Properties = {
    let prop_len = MQTT_VarLen;
    let start_pos = offset;
    while (offset - start_pos) < prop_len {
        let id = byte;
        case id of {
            0x01u -> byte;
            0x02u -> uint32;
            0x03u -> MQTT_String;
            0x08u -> MQTT_String;
            0x09u -> { let len = uint16; bytes(len) };
            0x0Bu -> byte;
            0x11u -> uint32;
            0x12u -> MQTT_String;
            0x13u -> uint16;
            0x15u -> MQTT_String;
            0x16u -> { let len = uint16; bytes(len) };
            0x17u -> byte;
            0x18u -> uint32;
            0x19u -> byte;
            0x1Au -> MQTT_String;
            0x1Cu -> MQTT_String;
            0x1Fu -> MQTT_String;
            0x21u -> uint16;
            0x22u -> uint16;
            0x23u -> uint16;
            0x24u -> byte;
            0x25u -> byte;
            0x26u -> { MQTT_String; MQTT_String };
            0x27u -> uint32;
            0x28u -> byte;
            0x29u -> byte;
            0x2Au -> byte;
            _ -> fail
        }
    }
}

def MQTT_Packet = {
    let first_byte = byte;
    let packet_type = (first_byte >> 4u);
    let flags = (first_byte & 0x0Fu);
    let remaining_length = MQTT_VarLen;
    let end_pos = offset + remaining_length;

    case packet_type of {
        1u -> {
            MQTT_String;
            byte;
            let connect_flags = byte;
            uint16;
            MQTT_Properties;
            if ((connect_flags & 0x80u) != 0u) { MQTT_String };
            if ((connect_flags & 0x40u) != 0u) { MQTT_String };
            if ((connect_flags & 0x04u) != 0u) {
                MQTT_Properties;
                MQTT_String;
                let len = uint16;
                bytes(len)
            }
        };
        2u -> {
            byte;
            byte;
            MQTT_Properties
        };
        3u -> {
            MQTT_String;
            if ((flags & 0x06u) > 0u) { uint16 };
            MQTT_Properties;
            bytes(end_pos - offset)
        };
        4u -> uint16;
        5u -> uint16;
        6u -> uint16;
        7u -> uint16;
        8u -> {
            while offset < end_pos {
                MQTT_String;
                byte
            }
        };
        9u -> bytes(remaining_length);
        10u -> {
            while offset < end_pos {
                MQTT_String
            }
        };
        11u -> bytes(remaining_length);
        12u -> null;
        13u -> null;
        14u -> {
            byte;
            MQTT_Properties
        };
        15u -> {
            byte;
            MQTT_Properties
        };
        _ -> fail
    }
}

def MQTT_Stream = {
    many MQTT_Packet
}

public MQTT_Stream