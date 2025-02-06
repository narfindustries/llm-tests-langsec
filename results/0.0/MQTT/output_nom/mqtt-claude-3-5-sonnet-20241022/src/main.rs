use nom::{
    bits::complete::take,
    bytes::complete::{take as take_bytes, take_while},
    combinator::{map, verify},
    multi::length_data,
    number::complete::{be_u16, be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::{env, fs::File, io::Read};

#[derive(Debug)]
pub enum PacketType {
    Connect = 1,
    Connack = 2,
    Publish = 3,
    Puback = 4,
    Pubrec = 5,
    Pubrel = 6,
    Pubcomp = 7,
    Subscribe = 8,
    Suback = 9,
    Unsubscribe = 10,
    Unsuback = 11,
    Pingreq = 12,
    Pingresp = 13,
    Disconnect = 14,
    Auth = 15,
}

#[derive(Debug)]
pub struct FixedHeader {
    packet_type: PacketType,
    dup_flag: bool,
    qos_level: u8,
    retain: bool,
    remaining_length: u32,
}

#[derive(Debug)]
pub struct ConnectFlags {
    clean_start: bool,
    will_flag: bool,
    will_qos: u8,
    will_retain: bool,
    password_flag: bool,
    username_flag: bool,
}

#[derive(Debug)]
pub struct Property {
    identifier: u8,
    value: PropertyValue,
}

#[derive(Debug)]
pub enum PropertyValue {
    Byte(u8),
    TwoByteInteger(u16),
    FourByteInteger(u32),
    VariableByteInteger(u32),
    UTF8String(String),
    BinaryData(Vec<u8>),
    UTF8StringPair(String, String),
}

#[derive(Debug)]
pub struct MQTTPacket {
    fixed_header: FixedHeader,
    variable_header: Option<Vec<u8>>,
    payload: Option<Vec<u8>>,
}

fn parse_remaining_length(input: &[u8]) -> IResult<&[u8], u32> {
    let mut multiplier: u32 = 1;
    let mut value: u32 = 0;
    let mut current_byte: u8;
    let mut bytes_read = 0;
    let mut remaining = input;

    loop {
        let (rest, byte) = be_u8(remaining)?;
        remaining = rest;
        current_byte = byte;
        value += ((current_byte & 0x7F) as u32) * multiplier;
        multiplier *= 128;
        bytes_read += 1;

        if current_byte & 0x80 == 0 || bytes_read >= 4 {
            break;
        }
    }

    Ok((remaining, value))
}

fn parse_fixed_header(input: &[u8]) -> IResult<&[u8], FixedHeader> {
    let (input, first_byte) = be_u8(input)?;
    let packet_type = match first_byte >> 4 {
        1 => PacketType::Connect,
        2 => PacketType::Connack,
        3 => PacketType::Publish,
        4 => PacketType::Puback,
        5 => PacketType::Pubrec,
        6 => PacketType::Pubrel,
        7 => PacketType::Pubcomp,
        8 => PacketType::Subscribe,
        9 => PacketType::Suback,
        10 => PacketType::Unsubscribe,
        11 => PacketType::Unsuback,
        12 => PacketType::Pingreq,
        13 => PacketType::Pingresp,
        14 => PacketType::Disconnect,
        15 => PacketType::Auth,
        _ => return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    };

    let flags = first_byte & 0x0F;
    let (input, remaining_length) = parse_remaining_length(input)?;

    Ok((
        input,
        FixedHeader {
            packet_type,
            dup_flag: (flags & 0x08) != 0,
            qos_level: (flags & 0x06) >> 1,
            retain: (flags & 0x01) != 0,
            remaining_length,
        },
    ))
}

fn parse_utf8_string(input: &[u8]) -> IResult<&[u8], String> {
    let (input, len) = be_u16(input)?;
    let (input, bytes) = take_bytes(len as usize)(input)?;
    Ok((
        input,
        String::from_utf8(bytes.to_vec()).unwrap_or_default(),
    ))
}

fn parse_property(input: &[u8]) -> IResult<&[u8], Property> {
    let (input, identifier) = be_u8(input)?;
    let (input, value) = match identifier {
        0x01 | 0x24 | 0x25 | 0x28 | 0x29 | 0x2A => {
            let (input, val) = be_u8(input)?;
            (input, PropertyValue::Byte(val))
        }
        0x21 | 0x22 | 0x23 => {
            let (input, val) = be_u16(input)?;
            (input, PropertyValue::TwoByteInteger(val))
        }
        0x02 | 0x11 | 0x27 => {
            let (input, val) = be_u32(input)?;
            (input, PropertyValue::FourByteInteger(val))
        }
        0x0B => {
            let (input, val) = parse_remaining_length(input)?;
            (input, PropertyValue::VariableByteInteger(val))
        }
        0x03 | 0x08 | 0x12 | 0x15 | 0x1A | 0x1C | 0x1F => {
            let (input, val) = parse_utf8_string(input)?;
            (input, PropertyValue::UTF8String(val))
        }
        0x09 | 0x16 => {
            let (input, len) = be_u16(input)?;
            let (input, data) = take_bytes(len as usize)(input)?;
            (input, PropertyValue::BinaryData(data.to_vec()))
        }
        0x26 => {
            let (input, key) = parse_utf8_string(input)?;
            let (input, value) = parse_utf8_string(input)?;
            (input, PropertyValue::UTF8StringPair(key, value))
        }
        _ => return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    };

    Ok((input, Property { identifier, value }))
}

fn parse_mqtt_packet(input: &[u8]) -> IResult<&[u8], MQTTPacket> {
    let (input, fixed_header) = parse_fixed_header(input)?;
    let (input, variable_header_and_payload) =
        take_bytes(fixed_header.remaining_length as usize)(input)?;

    Ok((
        input,
        MQTTPacket {
            fixed_header,
            variable_header: Some(variable_header_and_payload.to_vec()),
            payload: None,
        },
    ))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <mqtt_binary_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_mqtt_packet(&buffer) {
        Ok((remaining, packet)) => {
            println!("Parsed MQTT packet: {:?}", packet);
            println!("Remaining unparsed bytes: {} bytes", remaining.len());
        }
        Err(e) => eprintln!("Failed to parse MQTT packet: {:?}", e),
    }

    Ok(())
}