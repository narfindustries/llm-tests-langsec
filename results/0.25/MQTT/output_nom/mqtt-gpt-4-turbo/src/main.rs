use nom::{
    bytes::complete::{take, tag},
    combinator::{map, map_res},
    multi::count,
    number::complete::{be_u16, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::{env, fs::File, io::Read};

#[derive(Debug)]
enum QoS {
    AtMostOnce,
    AtLeastOnce,
    ExactlyOnce,
}

#[derive(Debug)]
enum ControlPacketType {
    Connect,
    Connack,
    Publish,
    Puback,
    Pubrec,
    Pubrel,
    Pubcomp,
    Subscribe,
    Suback,
    Unsubscribe,
    Unsuback,
    Pingreq,
    Pingresp,
    Disconnect,
    Auth,
}

#[derive(Debug)]
struct FixedHeader {
    packet_type: ControlPacketType,
    flags: u8,
    remaining_length: usize,
}

#[derive(Debug)]
struct ConnectPacket {
    protocol_name: String,
    protocol_level: u8,
    connect_flags: u8,
    keep_alive: u16,
    client_id: String,
    will_topic: Option<String>,
    will_message: Option<Vec<u8>>,
    username: Option<String>,
    password: Option<Vec<u8>>,
}

fn parse_qos(i: &[u8]) -> IResult<&[u8], QoS> {
    let (i, qos) = be_u8(i)?;
    match qos {
        0 => Ok((i, QoS::AtMostOnce)),
        1 => Ok((i, QoS::AtLeastOnce)),
        2 => Ok((i, QoS::ExactlyOnce)),
        _ => Err(nom::Err::Error((i, nom::error::ErrorKind::Tag))),
    }
}

fn parse_string(i: &[u8]) -> IResult<&[u8], String> {
    let (i, len) = be_u16(i)?;
    map_res(take(len), |s: &[u8]| std::str::from_utf8(s).map(String::from))(i)
}

fn parse_connect_packet(i: &[u8]) -> IResult<&[u8], ConnectPacket> {
    let (i, protocol_name) = parse_string(i)?;
    let (i, protocol_level) = be_u8(i)?;
    let (i, connect_flags) = be_u8(i)?;
    let (i, keep_alive) = be_u16(i)?;
    let (i, client_id) = parse_string(i)?;

    let (i, will_topic) = if connect_flags & 0x04 > 0 {
        map(parse_string, Some)(i)?
    } else {
        (i, None)
    };

    let (i, will_message) = if connect_flags & 0x04 > 0 {
        map(parse_string, |s| Some(s.into_bytes()))(i)?
    } else {
        (i, None)
    };

    let (i, username) = if connect_flags & 0x80 > 0 {
        map(parse_string, Some)(i)?
    } else {
        (i, None)
    };

    let (i, password) = if connect_flags & 0x40 > 0 {
        map(parse_string, |s| Some(s.into_bytes()))(i)?
    } else {
        (i, None)
    };

    Ok((
        i,
        ConnectPacket {
            protocol_name,
            protocol_level,
            connect_flags,
            keep_alive,
            client_id,
            will_topic,
            will_message,
            username,
            password,
        },
    ))
}

fn parse_fixed_header(i: &[u8]) -> IResult<&[u8], FixedHeader> {
    let (i, byte1) = be_u8(i)?;
    let packet_type = match byte1 >> 4 {
        1 => ControlPacketType::Connect,
        2 => ControlPacketType::Connack,
        3 => ControlPacketType::Publish,
        4 => ControlPacketType::Puback,
        5 => ControlPacketType::Pubrec,
        6 => ControlPacketType::Pubrel,
        7 => ControlPacketType::Pubcomp,
        8 => ControlPacketType::Subscribe,
        9 => ControlPacketType::Suback,
        10 => ControlPacketType::Unsubscribe,
        11 => ControlPacketType::Unsuback,
        12 => ControlPacketType::Pingreq,
        13 => ControlPacketType::Pingresp,
        14 => ControlPacketType::Disconnect,
        15 => ControlPacketType::Auth,
        _ => return Err(nom::Err::Error((i, nom::error::ErrorKind::Tag))),
    };
    let flags = byte1 & 0x0F;
    let (i, remaining_length) = be_u8(i)?; // Simplified, real implementation should handle variable length

    Ok((
        i,
        FixedHeader {
            packet_type,
            flags,
            remaining_length: remaining_length as usize,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Usage: {} <file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_fixed_header(&buffer) {
        Ok((i, header)) => {
            println!("Parsed Fixed Header: {:?}", header);
            match header.packet_type {
                ControlPacketType::Connect => {
                    if let Ok((_, connect_packet)) = parse_connect_packet(i) {
                        println!("Parsed Connect Packet: {:?}", connect_packet);
                    }
                }
                _ => println!("Other packet types not implemented"),
            }
        }
        Err(e) => println!("Failed to parse: {:?}", e),
    }
}