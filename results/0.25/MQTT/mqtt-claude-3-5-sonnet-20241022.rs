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
    username_flag: bool,
    password_flag: bool,
    will_retain: bool,
    will_qos: u8,
    will_flag: bool,
    clean_start: bool,
}

#[derive(Debug)]
pub struct Property {
    identifier: u8,
    value: Vec<u8>,
}

#[derive(Debug)]
pub struct ConnectPacket {
    fixed_header: FixedHeader,
    protocol_name: String,
    protocol_version: u8,
    connect_flags: ConnectFlags,
    keep_alive: u16,
    properties: Vec<Property>,
    client_id: String,
    will_properties: Option<Vec<Property>>,
    will_topic: Option<String>,
    will_payload: Option<Vec<u8>>,
    username: Option<String>,
    password: Option<Vec<u8>>,
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

fn parse_connect_flags(input: &[u8]) -> IResult<&[u8], ConnectFlags> {
    let (input, flags) = be_u8(input)?;
    Ok((
        input,
        ConnectFlags {
            username_flag: (flags & 0x80) != 0,
            password_flag: (flags & 0x40) != 0,
            will_retain: (flags & 0x20) != 0,
            will_qos: (flags & 0x18) >> 3,
            will_flag: (flags & 0x04) != 0,
            clean_start: (flags & 0x02) != 0,
        },
    ))
}

fn parse_property(input: &[u8]) -> IResult<&[u8], Property> {
    let (input, identifier) = be_u8(input)?;
    let (input, len) = parse_remaining_length(input)?;
    let (input, value) = take_bytes(len as usize)(input)?;
    
    Ok((
        input,
        Property {
            identifier,
            value: value.to_vec(),
        },
    ))
}

fn parse_properties(input: &[u8]) -> IResult<&[u8], Vec<Property>> {
    let (input, props_len) = parse_remaining_length(input)?;
    let (input, props_data) = take_bytes(props_len as usize)(input)?;
    
    let mut properties = Vec::new();
    let mut remaining = props_data;
    
    while !remaining.is_empty() {
        let (rest, property) = parse_property(remaining)?;
        properties.push(property);
        remaining = rest;
    }
    
    Ok((input, properties))
}

fn parse_connect(input: &[u8]) -> IResult<&[u8], ConnectPacket> {
    let (input, fixed_header) = parse_fixed_header(input)?;
    let (input, protocol_name) = parse_utf8_string(input)?;
    let (input, protocol_version) = be_u8(input)?;
    let (input, connect_flags) = parse_connect_flags(input)?;
    let (input, keep_alive) = be_u16(input)?;
    let (input, properties) = parse_properties(input)?;
    let (input, client_id) = parse_utf8_string(input)?;

    let (input, will_properties, will_topic, will_payload) = if connect_flags.will_flag {
        let (input, props) = parse_properties(input)?;
        let (input, topic) = parse_utf8_string(input)?;
        let (input, payload_len) = be_u16(input)?;
        let (input, payload) = take_bytes(payload_len as usize)(input)?;
        (input, Some(props), Some(topic), Some(payload.to_vec()))
    } else {
        (input, None, None, None)
    };

    let (input, username) = if connect_flags.username_flag {
        let (input, name) = parse_utf8_string(input)?;
        (input, Some(name))
    } else {
        (input, None)
    };

    let (input, password) = if connect_flags.password_flag {
        let (input, pass_len) = be_u16(input)?;
        let (input, pass) = take_bytes(pass_len as usize)(input)?;
        (input, Some(pass.to_vec()))
    } else {
        (input, None)
    };

    Ok((
        input,
        ConnectPacket {
            fixed_header,
            protocol_name,
            protocol_version,
            connect_flags,
            keep_alive,
            properties,
            client_id,
            will_properties,
            will_topic,
            will_payload,
            username,
            password,
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

    match parse_connect(&buffer) {
        Ok((remaining, packet)) => {
            println!("Parsed MQTT packet: {:#?}", packet);
            println!("Remaining bytes: {} bytes", remaining.len());
        }
        Err(e) => {
            eprintln!("Failed to parse MQTT packet: {:?}", e);
        }
    }

    Ok(())
}