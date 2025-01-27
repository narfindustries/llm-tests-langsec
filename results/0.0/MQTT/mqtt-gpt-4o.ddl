namespace MQTT

// Define the MQTT packet structure
struct MQTTPacket {
    fixed_header: FixedHeader,
    variable_header: VariableHeader,
    payload: Payload
}

// Define the fixed header structure
struct FixedHeader {
    byte1: u8,
    remaining_length: varint
}

// Define the variable header structure
struct VariableHeader {
    packet_identifier: u16,
    properties: Properties
}

// Define the properties structure
struct Properties {
    property_length: varint,
    properties: list of Property
}

// Define a single property structure
struct Property {
    id: u8,
    value: PropertyValue
}

// Define the property value as a union of possible types
union PropertyValue {
    0x01: u8,    // Byte
    0x02: u16,   // Two-byte integer
    0x03: u32,   // Four-byte integer
    0x04: u64,   // Eight-byte integer
    0x05: string // UTF-8 encoded string
}

// Define the payload structure
struct Payload {
    data: list of u8
}

// Define a variable-length integer (varint) type
type varint = {
    value: u32,
    _parse: {
        let mut multiplier = 1;
        let mut value = 0;
        loop {
            let byte = read_u8();
            value += (byte & 0x7F) * multiplier;
            if (byte & 0x80) == 0 {
                break;
            }
            multiplier *= 128;
        }
        return value;
    }
}
