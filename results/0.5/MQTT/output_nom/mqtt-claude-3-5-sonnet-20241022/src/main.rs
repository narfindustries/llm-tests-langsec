use nom::{
    bits::{bits, complete::take},
    bytes::complete::{take as take_bytes},
    combinator::map,
    number::complete::{be_u16, be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::{env, fs::File, io::Read};

#[derive(Debug)]
enum PacketType {
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
    AUTH = 15,
}

#[derive(Debug)]
struct FixedHeader {
    packet_type: PacketType,
    dup_flag: bool,
    qos_level: u8,
    retain: bool,
    remaining_length: u32,
}

#[derive(Debug)]
struct Property {
    identifier: u8,
    value: PropertyValue,
}

#[derive(Debug)]
enum PropertyValue {
    Byte(u8),
    TwoByteInteger(u16),
    FourByteInteger(u32),
    VariableByteInteger(u32),
    UTF8String(String),
    BinaryData(Vec<u8>),
    UTF8StringPair(String, String),
}

#[derive(Debug)]
struct MQTTPacket {
    fixed_header: FixedHeader,
    variable_header: Option<VariableHeader>,
    payload: Option<Vec<u8>>,
}

#[derive(Debug)]
struct VariableHeader {
    protocol_name: Option<String>,
    protocol_version: Option<u8>,
    connect_flags: Option<u8>,
    keep_alive: Option<u16>,
    properties: Vec<Property>,
    reason_code: Option<u8>,
}

fn parse_fixed_header(input: &[u8]) -> IResult<&[u8], FixedHeader> {
    let (input, (packet_type_bits, flags)): (&[u8], (u8, u8)) = bits::<_, _, nom::error::Error<(&[u8], usize)>, _, _>(
        tuple((take(4usize), take(4usize))),
    )(input)?;

    let packet_type = match packet_type_bits {
        1 => PacketType::CONNECT,
        2 => PacketType::CONNACK,
        3 => PacketType::PUBLISH,
        4 => PacketType::PUBACK,
        5 => PacketType::PUBREC,
        6 => PacketType::PUBREL,
        7 => PacketType::PUBCOMP,
        8 => PacketType::SUBSCRIBE,
        9 => PacketType::SUBACK,
        10 => PacketType::UNSUBSCRIBE,
        11 => PacketType::UNSUBACK,
        12 => PacketType::PINGREQ,
        13 => PacketType::PINGRESP,
        14 => PacketType::DISCONNECT,
        15 => PacketType::AUTH,
        _ => return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
    };

    let (input, remaining_length) = parse_variable_byte_integer(input)?;

    Ok((input, FixedHeader {
        packet_type,
        dup_flag: (flags & 0x08) != 0,
        qos_level: (flags & 0x06) >> 1,
        retain: (flags & 0x01) != 0,
        remaining_length,
    }))
}

fn parse_variable_byte_integer(input: &[u8]) -> IResult<&[u8], u32> {
    let mut value: u32 = 0;
    let mut multiplier: u32 = 1;
    let mut current_input = input;
    
    loop {
        let (next_input, byte) = be_u8(current_input)?;
        current_input = next_input;
        
        value += ((byte & 0x7F) as u32) * multiplier;
        multiplier *= 128;
        
        if byte & 0x80 == 0 {
            break;
        }
        
        if multiplier > 128 * 128 * 128 {
            return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::TooLarge)));
        }
    }
    
    Ok((current_input, value))
}

fn parse_utf8_string(input: &[u8]) -> IResult<&[u8], String> {
    let (input, len) = be_u16(input)?;
    let (input, data) = take_bytes(len as usize)(input)?;
    Ok((input, String::from_utf8_lossy(data).into_owned()))
}

fn parse_property(input: &[u8]) -> IResult<&[u8], Property> {
    let (input, identifier) = be_u8(input)?;
    let (input, value) = match identifier {
        1 | 23 | 25 | 36 | 37 | 40 | 41 | 42 => 
            map(be_u8, PropertyValue::Byte)(input),
        19 | 33 | 34 | 35 =>
            map(be_u16, PropertyValue::TwoByteInteger)(input),
        2 | 17 | 24 | 39 =>
            map(be_u32, PropertyValue::FourByteInteger)(input),
        11 =>
            map(parse_variable_byte_integer, PropertyValue::VariableByteInteger)(input),
        3 | 8 | 18 | 21 | 26 | 28 | 31 =>
            map(parse_utf8_string, PropertyValue::UTF8String)(input),
        9 | 22 => {
            let (input, len) = be_u16(input)?;
            let (input, data) = take_bytes(len as usize)(input)?;
            Ok((input, PropertyValue::BinaryData(data.to_vec())))
        },
        38 => {
            let (input, key) = parse_utf8_string(input)?;
            let (input, value) = parse_utf8_string(input)?;
            Ok((input, PropertyValue::UTF8StringPair(key, value)))
        },
        _ => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
    }?;

    Ok((input, Property { identifier, value }))
}

fn parse_properties(input: &[u8]) -> IResult<&[u8], Vec<Property>> {
    let (input, length) = parse_variable_byte_integer(input)?;
    let (input, properties_data) = take_bytes(length as usize)(input)?;
    
    let mut properties = Vec::new();
    let mut current_input = properties_data;
    
    while !current_input.is_empty() {
        let (next_input, property) = parse_property(current_input)?;
        properties.push(property);
        current_input = next_input;
    }
    
    Ok((input, properties))
}

fn parse_variable_header<'a>(input: &'a [u8], packet_type: &PacketType) -> IResult<&'a [u8], VariableHeader> {
    match packet_type {
        PacketType::CONNECT => {
            let (input, protocol_name) = parse_utf8_string(input)?;
            let (input, protocol_version) = be_u8(input)?;
            let (input, connect_flags) = be_u8(input)?;
            let (input, keep_alive) = be_u16(input)?;
            let (input, properties) = parse_properties(input)?;
            
            Ok((input, VariableHeader {
                protocol_name: Some(protocol_name),
                protocol_version: Some(protocol_version),
                connect_flags: Some(connect_flags),
                keep_alive: Some(keep_alive),
                properties,
                reason_code: None,
            }))
        },
        _ => {
            let (input, properties) = parse_properties(input)?;
            Ok((input, VariableHeader {
                protocol_name: None,
                protocol_version: None,
                connect_flags: None,
                keep_alive: None,
                properties,
                reason_code: None,
            }))
        }
    }
}

fn parse_mqtt_packet(input: &[u8]) -> IResult<&[u8], MQTTPacket> {
    let (input, fixed_header) = parse_fixed_header(input)?;
    let (input, variable_header) = parse_variable_header(input, &fixed_header.packet_type)?;
    
    let (input, payload) = if !input.is_empty() {
        let (input, payload_data) = take_bytes(input.len())(input)?;
        Ok((input, Some(payload_data.to_vec())))
    } else {
        Ok((input, None))
    }?;

    Ok((input, MQTTPacket {
        fixed_header,
        variable_header: Some(variable_header),
        payload,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <mqtt_binary_file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_mqtt_packet(&buffer) {
        Ok((remaining, packet)) => {
            println!("Parsed MQTT packet: {:?}", packet);
            if !remaining.is_empty() {
                println!("Warning: {} bytes remaining unparsed", remaining.len());
            }
        },
        Err(e) => eprintln!("Failed to parse MQTT packet: {:?}", e),
    }
}