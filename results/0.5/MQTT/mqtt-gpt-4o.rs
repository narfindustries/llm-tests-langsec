use std::fs::File;
use std::io::{self, Read};
use std::env;
use nom::{
    IResult,
    bytes::complete::{tag, take},
    number::complete::{be_u8, be_u16},
    combinator::{map, opt},
    sequence::{tuple, preceded},
    multi::length_data,
};

#[derive(Debug)]
enum MqttControlPacket {
    Connect { protocol_name: String, protocol_level: u8, flags: u8, keep_alive: u16, client_id: String },
    ConnAck { session_present: bool, return_code: u8 },
    Publish { topic_name: String, packet_id: Option<u16>, payload: Vec<u8> },
    // Add other packet types as needed
}

fn parse_mqtt_packet(input: &[u8]) -> IResult<&[u8], MqttControlPacket> {
    let (input, packet_type_flags) = be_u8(input)?;
    let packet_type = packet_type_flags >> 4;
    match packet_type {
        1 => parse_connect(input),
        2 => parse_connack(input),
        3 => parse_publish(input),
        _ => Err(nom::Err::Error((input, nom::error::ErrorKind::Tag))),
    }
}

fn parse_connect(input: &[u8]) -> IResult<&[u8], MqttControlPacket> {
    let (input, (_, protocol_name, protocol_level, flags, keep_alive, client_id)) = tuple((
        length_data(be_u8),
        map(length_data(be_u16), |s: &[u8]| String::from_utf8_lossy(s).to_string()),
        be_u8,
        be_u8,
        be_u16,
        map(length_data(be_u16), |s: &[u8]| String::from_utf8_lossy(s).to_string()),
    ))(input)?;
    Ok((input, MqttControlPacket::Connect {
        protocol_name,
        protocol_level,
        flags,
        keep_alive,
        client_id,
    }))
}

fn parse_connack(input: &[u8]) -> IResult<&[u8], MqttControlPacket> {
    let (input, (session_present, return_code)) = tuple((
        map(be_u8, |b| b & 0x01 != 0),
        be_u8,
    ))(input)?;
    Ok((input, MqttControlPacket::ConnAck {
        session_present,
        return_code,
    }))
}

fn parse_publish(input: &[u8]) -> IResult<&[u8], MqttControlPacket> {
    let (input, (topic_name, packet_id, payload)) = tuple((
        map(length_data(be_u16), |s: &[u8]| String::from_utf8_lossy(s).to_string()),
        opt(be_u16),
        length_data(be_u8),
    ))(input)?;
    Ok((input, MqttControlPacket::Publish {
        topic_name,
        packet_id,
        payload: payload.to_vec(),
    }))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: mqtt_parser <file>");
        return Ok(());
    }

    let filename = &args[1];
    let mut file = File::open(filename)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_mqtt_packet(&buffer) {
        Ok((_, packet)) => {
            println!("{:?}", packet);
        }
        Err(e) => {
            eprintln!("Failed to parse MQTT packet: {:?}", e);
        }
    }

    Ok(())
}