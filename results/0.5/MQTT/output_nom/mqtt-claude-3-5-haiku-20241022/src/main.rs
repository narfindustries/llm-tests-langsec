use nom::{
    bits::{streaming::take as take_bits, streaming::tag as tag_bits},
    bytes::streaming::{tag, take, take_while},
    combinator::{map, opt},
    error::ErrorKind,
    multi::{count, many0, many1},
    number::streaming::{be_u8, be_u16, be_u32},
    sequence::{pair, preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
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
    Auth,
}

#[derive(Debug)]
struct MqttPacket {
    packet_type: PacketType,
    flags: u8,
    remaining_length: usize,
    payload: Vec<u8>,
}

fn parse_fixed_header(input: &[u8]) -> IResult<&[u8], MqttPacket> {
    let (input, first_byte) = be_u8(input)?;
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
        15 => PacketType::Auth,
        _ => return Err(nom::Err::Error(nom::error::Error::new(input, ErrorKind::Tag))),
    };

    let flags = first_byte & 0x0F;

    let (input, remaining_length) = parse_variable_length(input)?;

    Ok((input, MqttPacket {
        packet_type,
        flags,
        remaining_length,
        payload: Vec::new(),
    }))
}

fn parse_variable_length(input: &[u8]) -> IResult<&[u8], usize> {
    let mut multiplier = 1;
    let mut value = 0;
    let mut bytes_read = 0;

    for (i, &byte) in input.iter().enumerate() {
        value += (byte & 0x7F) as usize * multiplier;
        multiplier *= 0x80;
        bytes_read += 1;

        if byte & 0x80 == 0 {
            return Ok((&input[bytes_read..], value));
        }

        if i >= 4 {
            return Err(nom::Err::Error(nom::error::Error::new(input, ErrorKind::LengthValue)));
        }
    }

    Err(nom::Err::Incomplete(nom::Needed::new(1)))
}

fn parse_connect_packet(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = tag(b"MQTT")(input)?;
    let (input, protocol_version) = be_u8(input)?;
    
    if protocol_version != 5 {
        return Err(nom::Err::Error(nom::error::Error::new(input, ErrorKind::Tag)));
    }

    let (input, connect_flags) = be_u8(input)?;
    let (input, keep_alive) = be_u16(input)?;

    // Parse properties (simplified)
    let (input, _properties_length) = parse_variable_length(input)?;

    Ok((input, ()))
}

fn parse_mqtt_packet(input: &[u8]) -> IResult<&[u8], MqttPacket> {
    let (input, mut packet) = parse_fixed_header(input)?;

    let (input, payload) = take(packet.remaining_length)(input)?;
    packet.payload = payload.to_vec();

    match packet.packet_type {
        PacketType::Connect => {
            let _ = parse_connect_packet(payload)?;
        },
        _ => {}
    }

    Ok((input, packet))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <mqtt_file>", args[0]);
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