use nom::{
    bits::complete::{tag as bits_tag, take as bits_take},
    bytes::complete::{tag, take},
    combinator::{map, verify},
    multi::{length_data, many0},
    number::complete::{be_u16, be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::{env, fs, io::Read};

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
    value: Vec<u8>,
}

#[derive(Debug)]
struct ConnectFlags {
    username_flag: bool,
    password_flag: bool,
    will_retain: bool,
    will_qos: u8,
    will_flag: bool,
    clean_start: bool,
}

#[derive(Debug)]
struct ConnectPacket {
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
    let mut current_input = input;
    let mut byte_count = 0;

    loop {
        let (rest, byte) = be_u8(current_input)?;
        current_input = rest;
        byte_count += 1;

        value += ((byte & 0x7F) as u32) * multiplier;
        multiplier *= 128;

        if byte & 0x80 == 0 || byte_count >= 4 {
            break;
        }
    }

    Ok((current_input, value))
}

fn parse_fixed_header(input: &[u8]) -> IResult<&[u8], FixedHeader> {
    let (input, (packet_type_bits, flags)) = bits::bits::<_, _, Error<(&[u8], usize)>, _, _>(
        tuple((
            bits_take(4usize),
            bits_take(4usize),
        ))
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
        _ => return Err(nom::Err::Error(Error::new(input, ErrorKind::Tag))),
    };

    let (input, remaining_length) = parse_remaining_length(input)?;

    Ok((input, FixedHeader {
        packet_type,
        dup_flag: (flags & 0x08) != 0,
        qos_level: (flags & 0x06) >> 1,
        retain: (flags & 0x01) != 0,
        remaining_length,
    }))
}

fn parse_utf8_string(input: &[u8]) -> IResult<&[u8], String> {
    let (input, len) = be_u16(input)?;
    let (input, string_bytes) = take(len)(input)?;
    Ok((input, String::from_utf8_lossy(string_bytes).into_owned()))
}

fn parse_property(input: &[u8]) -> IResult<&[u8], Property> {
    let (input, identifier) = be_u8(input)?;
    let (input, value) = match identifier {
        0x01 | 0x17 | 0x24 | 0x25 | 0x28 | 0x29 | 0x2A => {
            let (input, val) = be_u8(input)?;
            (input, vec![val])
        },
        0x02 | 0x11 | 0x21 | 0x27 => {
            let (input, val) = be_u32(input)?;
            (input, val.to_be_bytes().to_vec())
        },
        0x22 | 0x23 => {
            let (input, val) = be_u16(input)?;
            (input, val.to_be_bytes().to_vec())
        },
        _ => {
            let (input, str_val) = parse_utf8_string(input)?;
            (input, str_val.into_bytes())
        },
    };
    
    Ok((input, Property { identifier, value }))
}

fn parse_properties(input: &[u8]) -> IResult<&[u8], Vec<Property>> {
    let (input, length) = parse_remaining_length(input)?;
    let (input, props) = many0(parse_property)(input)?;
    Ok((input, props))
}

fn parse_connect_flags(input: &[u8]) -> IResult<&[u8], ConnectFlags> {
    let (input, flags) = be_u8(input)?;
    Ok((input, ConnectFlags {
        username_flag: (flags & 0x80) != 0,
        password_flag: (flags & 0x40) != 0,
        will_retain: (flags & 0x20) != 0,
        will_qos: (flags & 0x18) >> 3,
        will_flag: (flags & 0x04) != 0,
        clean_start: (flags & 0x02) != 0,
    }))
}

fn parse_connect(input: &[u8]) -> IResult<&[u8], ConnectPacket> {
    let (input, protocol_name) = parse_utf8_string(input)?;
    let (input, protocol_version) = verify(be_u8, |&x| x == 5)(input)?;
    let (input, connect_flags) = parse_connect_flags(input)?;
    let (input, keep_alive) = be_u16(input)?;
    let (input, properties) = parse_properties(input)?;
    let (input, client_id) = parse_utf8_string(input)?;

    let (input, will_properties, will_topic, will_payload) = if connect_flags.will_flag {
        let (input, props) = parse_properties(input)?;
        let (input, topic) = parse_utf8_string(input)?;
        let (input, payload) = length_data(be_u16)(input)?;
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
        let (input, pass) = length_data(be_u16)(input)?;
        (input, Some(pass.to_vec()))
    } else {
        (input, None)
    };

    Ok((input, ConnectPacket {
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
    }))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <mqtt_binary_file>", args[0]);
        return Ok(());
    }

    let mut file = fs::File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_fixed_header(&buffer) {
        Ok((remaining, header)) => {
            println!("Fixed Header: {:?}", header);
            match header.packet_type {
                PacketType::CONNECT => {
                    match parse_connect(remaining) {
                        Ok((_, packet)) => println!("Connect Packet: {:?}", packet),
                        Err(e) => println!("Error parsing CONNECT: {:?}", e),
                    }
                },
                _ => println!("Parsing for packet type {:?} not implemented", header.packet_type),
            }
        },
        Err(e) => println!("Error parsing fixed header: {:?}", e),
    }

    Ok(())
}