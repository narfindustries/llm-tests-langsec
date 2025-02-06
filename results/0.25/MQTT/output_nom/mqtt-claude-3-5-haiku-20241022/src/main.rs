use nom::{
    bits::{streaming::take as take_bits, streaming::tag as tag_bits},
    branch::alt,
    bytes::streaming::{tag, take},
    combinator::{cond, map, opt, verify},
    error::{ErrorKind, ParseError},
    multi::{count, many0, many_m_n},
    number::streaming::{be_u8, be_u16, be_u32},
    sequence::{pair, preceded, tuple},
    IResult, Err,
};
use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug, Clone)]
enum PacketType {
    Connect,
    ConnAck,
    Publish,
    PubAck,
    PubRec,
    PubRel,
    PubComp,
    Subscribe,
    SubAck,
    Unsubscribe,
    UnsubAck,
    PingReq,
    PingResp,
    Disconnect,
}

#[derive(Debug, Clone)]
struct MqttPacket {
    packet_type: PacketType,
    flags: u8,
    remaining_length: usize,
    payload: Vec<u8>,
}

#[derive(Debug, Clone)]
struct ConnectPacket {
    protocol_name: String,
    protocol_version: u8,
    connect_flags: u8,
    keep_alive: u16,
    properties: HashMap<String, Vec<u8>>,
    client_id: String,
    username: Option<String>,
    password: Option<String>,
}

fn parse_variable_length_integer(input: &[u8]) -> IResult<&[u8], usize> {
    let mut multiplier = 1;
    let mut value = 0;
    let mut bytes_read = 0;

    loop {
        let (remaining, byte) = be_u8(input)?;
        value += (byte & 0x7F) as usize * multiplier;
        multiplier *= 128;
        bytes_read += 1;

        if byte & 0x80 == 0 {
            return Ok((remaining, value));
        }

        if bytes_read > 4 {
            return Err(Err::Error(ParseError::from_error_kind(input, ErrorKind::TooLarge)));
        }
    }
}

fn parse_mqtt_fixed_header(input: &[u8]) -> IResult<&[u8], MqttPacket> {
    let (remaining, first_byte) = be_u8(input)?;
    let packet_type = match first_byte >> 4 {
        1 => PacketType::Connect,
        2 => PacketType::ConnAck,
        3 => PacketType::Publish,
        4 => PacketType::PubAck,
        5 => PacketType::PubRec,
        6 => PacketType::PubRel,
        7 => PacketType::PubComp,
        8 => PacketType::Subscribe,
        9 => PacketType::SubAck,
        10 => PacketType::Unsubscribe,
        11 => PacketType::UnsubAck,
        12 => PacketType::PingReq,
        13 => PacketType::PingResp,
        14 => PacketType::Disconnect,
        _ => return Err(Err::Error(ParseError::from_error_kind(input, ErrorKind::Tag))),
    };

    let flags = first_byte & 0x0F;
    let (remaining, remaining_length) = parse_variable_length_integer(remaining)?;

    Ok((remaining, MqttPacket {
        packet_type,
        flags,
        remaining_length,
        payload: Vec::new(),
    }))
}

fn parse_connect_packet(input: &[u8]) -> IResult<&[u8], ConnectPacket> {
    let (remaining, protocol_name) = map(
        verify(take(6usize), |s: &[u8]| s == b"MQTT\x05"),
        |s| String::from_utf8_lossy(s).into_owned()
    )(input)?;

    let (remaining, protocol_version) = be_u8(remaining)?;
    let (remaining, connect_flags) = be_u8(remaining)?;
    let (remaining, keep_alive) = be_u16(remaining)?;

    let (remaining, properties) = parse_properties(remaining)?;
    let (remaining, client_id) = parse_utf8_string(remaining)?;

    let username = if connect_flags & 0x80 != 0 {
        let (remaining, username) = parse_utf8_string(remaining)?;
        Some(username)
    } else {
        None
    };

    let password = if connect_flags & 0x40 != 0 {
        let (remaining, password) = parse_utf8_string(remaining)?;
        Some(password)
    } else {
        None
    };

    Ok((remaining, ConnectPacket {
        protocol_name,
        protocol_version,
        connect_flags,
        keep_alive,
        properties,
        client_id,
        username,
        password,
    }))
}

fn parse_properties(input: &[u8]) -> IResult<&[u8], HashMap<String, Vec<u8>>> {
    let (remaining, length) = parse_variable_length_integer(input)?;
    let (remaining, properties) = many0(parse_property)(remaining)?;
    Ok((remaining, properties.into_iter().collect()))
}

fn parse_property(input: &[u8]) -> IResult<&[u8], (String, Vec<u8>)> {
    let (remaining, property_id) = be_u8(input)?;
    match property_id {
        0x21 => {
            let (remaining, value) = be_u32(remaining)?;
            Ok((remaining, ("Session Expiry Interval".to_string(), value.to_be_bytes().to_vec())))
        },
        _ => {
            let (remaining, value) = parse_utf8_string(remaining)?;
            Ok((remaining, (format!("Unknown Property {}", property_id), value.into_bytes())))
        }
    }
}

fn parse_utf8_string(input: &[u8]) -> IResult<&[u8], String> {
    let (remaining, length) = be_u16(input)?;
    let (remaining, string_bytes) = take(length as usize)(remaining)?;
    Ok((remaining, String::from_utf8_lossy(string_bytes).into_owned()))
}

fn parse_mqtt_packet(input: &[u8]) -> IResult<&[u8], MqttPacket> {
    let (remaining, mut packet) = parse_mqtt_fixed_header(input)?;
    
    match packet.packet_type {
        PacketType::Connect => {
            let (remaining, connect_packet) = parse_connect_packet(remaining)?;
            packet.payload = remaining.to_vec();
            Ok((remaining, packet))
        },
        _ => Ok((remaining, packet))
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_mqtt_packet(&buffer) {
        Ok((_, packet)) => {
            println!("Parsed MQTT Packet: {:?}", packet);
            Ok(())
        },
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1);
        }
    }
}