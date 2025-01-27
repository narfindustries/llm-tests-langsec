use nom::bytes::complete::{tag, take};
use nom::combinator::map_res;
use nom::number::complete::{be_u16, be_u8};
use nom::sequence::{pair, tuple};
use nom::IResult;
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug)]
enum MqttPacket {
    Connect { 
        protocol_name: String, 
        protocol_level: u8, 
        connect_flags: u8, 
        keep_alive: u16, 
        client_id: String 
    },
    ConnAck { 
        session_present: u8, 
        return_code: u8 
    },
    Publish {
        topic_name: String,
        packet_id: Option<u16>,
        payload: Vec<u8>,
    },
    // Add additional packet types as per specification
}

fn parse_string(input: &[u8]) -> IResult<&[u8], String> {
    let (input, length) = be_u16(input)?;
    let (input, string_bytes) = take(length)(input)?;
    let string_res = String::from_utf8(string_bytes.to_vec());
    map_res(string_res, |s| Ok::<_, std::string::FromUtf8Error>(s))(input)
}

fn parse_connect(input: &[u8]) -> IResult<&[u8], MqttPacket> {
    let (input, (_, protocol_name, protocol_level, connect_flags, keep_alive, client_id)) = tuple((
        tag([0x10]), // Packet type for CONNECT
        parse_string,
        be_u8,
        be_u8,
        be_u16,
        parse_string,
    ))(input)?;
    Ok((
        input,
        MqttPacket::Connect {
            protocol_name,
            protocol_level,
            connect_flags,
            keep_alive,
            client_id,
        },
    ))
}

fn parse_connack(input: &[u8]) -> IResult<&[u8], MqttPacket> {
    let (input, (_, session_present, return_code)) = tuple((
        tag([0x20]),
        be_u8,
        be_u8,
    ))(input)?;
    Ok((input, MqttPacket::ConnAck { session_present, return_code }))
}

fn parse_publish(input: &[u8]) -> IResult<&[u8], MqttPacket> {
    let (input, (_, topic_name, packet_id, payload)) = tuple((
        tag([0x30]), // Simplified, assuming QoS 0 for no packet_id
        parse_string,
        be_u16,
        take(remaining_length(input)?),
    ))(input)?;
    Ok((input, MqttPacket::Publish { topic_name, packet_id: Some(packet_id), payload: payload.to_vec() }))
}

// A utility function to calculate remaining length of packet if needed
fn remaining_length(input: &[u8]) -> IResult<&[u8], usize> {
    // This function should decode the MQTT remaining length encoding
    // For simplicity, let's assume a single byte length here, which is not always correct for real MQTT
    let (input, length) = be_u8(input)?;
    Ok((input, length as usize))
}

fn parse_mqtt_packet(input: &[u8]) -> IResult<&[u8], MqttPacket> {
    let (input, packet) = nom::branch::alt((
        parse_connect,
        parse_connack,
        parse_publish,
        // Add more parsers for other MQTT packet types here
    ))(input)?;
    Ok((input, packet))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let mut file = match File::open(filename) {
        Ok(file) => file,
        Err(e) => {
            eprintln!("Failed to open file {}: {}", filename, e);
            std::process::exit(1);
        }
    };

    let mut buffer = Vec::new();
    if let Err(e) = file.read_to_end(&mut buffer) {
        eprintln!("Failed to read file {}: {}", filename, e);
        std::process::exit(1);
    }

    match parse_mqtt_packet(&buffer) {
        Ok((_, packet)) => println!("{:?}", packet),
        Err(e) => eprintln!("Failed to parse MQTT packet: {:?}", e),
    }
}