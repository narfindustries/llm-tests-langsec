use nom::{
    bytes::complete::{take, take_while_m_n},
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug, PartialEq)]
struct MqttConnect {
    protocol_name: String,
    protocol_level: u8,
    connect_flags: u8,
    keep_alive: u16,
    client_id: String,
}

#[derive(Debug, PartialEq)]
enum MqttPacket {
    Connect(MqttConnect),
    // Other packet types like Publish, Subscribe, etc., can be added here.
}

fn parse_string(input: &[u8]) -> IResult<&[u8], String> {
    let (input, len) = be_u16(input)?;
    let (input, raw_str) = take(len)(input)?;
    match std::str::from_utf8(raw_str) {
        Ok(s) => Ok((input, s.to_string())),
        Err(_) => Err(nom::Err::Error((input, nom::error::ErrorKind::Fail))),
    }
}

fn parse_connect_packet(input: &[u8]) -> IResult<&[u8], MqttPacket> {
    let (input, protocol_name) = parse_string(input)?;
    let (input, protocol_level) = be_u8(input)?;
    let (input, connect_flags) = be_u8(input)?;
    let (input, keep_alive) = be_u16(input)?;
    let (input, client_id) = parse_string(input)?;

    let connect = MqttConnect {
        protocol_name,
        protocol_level,
        connect_flags,
        keep_alive,
        client_id,
    };

    Ok((input, MqttPacket::Connect(connect)))
}

fn parse_mqtt(input: &[u8]) -> IResult<&[u8], MqttPacket> {
    let (input, packet_type) = be_u8(input)?;
    let (input, _) = be_u8(input)?; // Length, currently unused
    match packet_type {
        0x10 => parse_connect_packet(input),
        // Add cases for other MQTT packet types
        _ => Err(nom::Err::Error((input, nom::error::ErrorKind::Switch))),
    }
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_mqtt(&buffer) {
        Ok((_, packet)) => println!("{:?}", packet),
        Err(e) => eprintln!("Failed to parse MQTT packet: {:?}", e),
    }

    Ok(())
}