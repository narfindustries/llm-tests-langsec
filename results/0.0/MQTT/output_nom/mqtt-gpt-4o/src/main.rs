use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::length_data,
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
        client_id: String,
    },
    Connack {
        session_present: u8,
        reason_code: u8,
    },
    Publish {
        topic_name: String,
        packet_identifier: Option<u16>,
        payload: Vec<u8>,
    },
    // Add other packet types as needed
}

fn parse_string(input: &[u8]) -> IResult<&[u8], String> {
    map_res(length_data(be_u16), |bytes: &[u8]| {
        std::str::from_utf8(bytes).map(|s| s.to_string())
    })(input)
}

fn parse_connect(input: &[u8]) -> IResult<&[u8], MqttPacket> {
    let (input, (_, protocol_level, connect_flags, keep_alive, client_id)) = tuple((
        tag("MQTT"),
        be_u8,
        be_u8,
        be_u16,
        parse_string,
    ))(input)?;

    Ok((
        input,
        MqttPacket::Connect {
            protocol_name: "MQTT".to_string(),
            protocol_level,
            connect_flags,
            keep_alive,
            client_id,
        },
    ))
}

fn parse_connack(input: &[u8]) -> IResult<&[u8], MqttPacket> {
    let (input, (session_present, reason_code)) = tuple((be_u8, be_u8))(input)?;
    Ok((
        input,
        MqttPacket::Connack {
            session_present,
            reason_code,
        },
    ))
}

fn parse_publish(input: &[u8]) -> IResult<&[u8], MqttPacket> {
    let (input, (topic_name, packet_identifier, payload)) = tuple((
        parse_string,
        map(be_u16, |id| if id == 0 { None } else { Some(id) }),
        take(input.len()), // Assume the rest is payload for simplicity
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
    match packet_type >> 4 {
        1 => parse_connect(input),
        2 => parse_connack(input),
        3 => parse_publish(input),
        _ => unimplemented!(), // Handle other packet types
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