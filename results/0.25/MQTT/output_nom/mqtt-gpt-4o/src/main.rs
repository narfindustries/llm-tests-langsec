use nom::{
    bytes::complete::{tag, take},
    combinator::opt,
    number::complete::{be_u16, be_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;
use std::path::Path;

#[derive(Debug)]
enum MqttPacket {
    Connect {
        protocol_name: String,
        protocol_level: u8,
        connect_flags: u8,
        keep_alive: u16,
    },
    Connack {
        session_present: bool,
        reason_code: u8,
    },
    Publish {
        topic_name: String,
        packet_identifier: Option<u16>,
        payload: Vec<u8>,
    },
    // Other packet types can be added here
}

fn parse_string(input: &[u8]) -> IResult<&[u8], String> {
    let (input, length) = be_u16(input)?;
    let (input, string_bytes) = take(length)(input)?;
    let string = std::str::from_utf8(string_bytes).map_err(|_| nom::Err::Failure(nom::error::Error::new(input, nom::error::ErrorKind::Fail)))?;
    Ok((input, string.to_string()))
}

fn parse_connect(input: &[u8]) -> IResult<&[u8], MqttPacket> {
    let (input, (_, protocol_name, protocol_level, connect_flags, keep_alive)) = tuple((
        tag(&[0x10]), // Packet type for CONNECT
        parse_string,
        be_u8,
        be_u8,
        be_u16,
    ))(input)?;

    Ok((
        input,
        MqttPacket::Connect {
            protocol_name,
            protocol_level,
            connect_flags,
            keep_alive,
        },
    ))
}

fn parse_connack(input: &[u8]) -> IResult<&[u8], MqttPacket> {
    let (input, (_, session_present, reason_code)) = tuple((
        tag(&[0x20]), // Packet type for CONNACK
        be_u8,
        be_u8,
    ))(input)?;

    Ok((
        input,
        MqttPacket::Connack {
            session_present: session_present & 0x01 != 0,
            reason_code,
        },
    ))
}

fn parse_publish(input: &[u8]) -> IResult<&[u8], MqttPacket> {
    let (input, (_, topic_name, packet_identifier, payload)) = tuple((
        tag(&[0x30]), // Packet type for PUBLISH
        parse_string,
        opt(be_u16),
        take(input.len()), // Remaining bytes as payload
    ))(input)?;

    Ok((
        input,
        MqttPacket::Publish {
            topic_name,
            packet_identifier,
            payload: payload.to_vec(),
        },
    ))
}

fn parse_mqtt_packet(input: &[u8]) -> IResult<&[u8], MqttPacket> {
    let (input, packet_type) = be_u8(input)?;
    match packet_type {
        0x10 => parse_connect(input),
        0x20 => parse_connack(input),
        0x30 => parse_publish(input),
        _ => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return;
    }

    let path = Path::new(&args[1]);
    let mut file = File::open(&path).expect("Unable to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Unable to read file");

    match parse_mqtt_packet(&buffer) {
        Ok((_, packet)) => println!("{:?}", packet),
        Err(e) => eprintln!("Failed to parse MQTT packet: {:?}", e),
    }
}