use nom::{
    bits::{streaming::take as take_bits, complete::take as take_bits_complete},
    bytes::streaming::{tag, take},
    combinator::{map, opt, peek},
    error::{ErrorKind, ParseError},
    multi::{count, many0, many_m_n},
    number::streaming::{be_u8, be_u16, be_u32},
    sequence::{preceded, tuple},
    IResult, Err,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug, PartialEq)]
enum MQTTPacketType {
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
struct MQTTPacket {
    packet_type: MQTTPacketType,
    flags: u8,
    remaining_length: usize,
    payload: Option<Vec<u8>>,
}

fn parse_mqtt_fixed_header(input: &[u8]) -> IResult<&[u8], MQTTPacket> {
    let (input, first_byte) = be_u8(input)?;
    let packet_type_num = first_byte >> 4;
    let flags = first_byte & 0x0F;

    let packet_type = match packet_type_num {
        1 => MQTTPacketType::Connect,
        2 => MQTTPacketType::ConnAck,
        3 => MQTTPacketType::Publish,
        4 => MQTTPacketType::PubAck,
        5 => MQTTPacketType::PubRec,
        6 => MQTTPacketType::PubRel,
        7 => MQTTPacketType::PubComp,
        8 => MQTTPacketType::Subscribe,
        9 => MQTTPacketType::SubAck,
        10 => MQTTPacketType::Unsubscribe,
        11 => MQTTPacketType::UnsubAck,
        12 => MQTTPacketType::PingReq,
        13 => MQTTPacketType::PingResp,
        14 => MQTTPacketType::Disconnect,
        15 => MQTTPacketType::Auth,
        _ => return Err(Err::Error(ParseError::from_error_kind(input, ErrorKind::Alt))),
    };

    let (input, remaining_length) = parse_variable_length_integer(input)?;

    Ok((input, MQTTPacket {
        packet_type,
        flags,
        remaining_length,
        payload: None,
    }))
}

fn parse_variable_length_integer(input: &[u8]) -> IResult<&[u8], usize> {
    let mut multiplier = 1;
    let mut value = 0;
    let mut bytes_read = 0;

    for (index, &byte) in input.iter().enumerate() {
        value += (byte & 0x7F) as usize * multiplier;
        multiplier *= 128;

        bytes_read += 1;

        if byte & 0x80 == 0 || index >= 3 {
            break;
        }
    }

    let (remaining_input, _) = take(bytes_read)(input)?;

    Ok((remaining_input, value))
}

fn parse_mqtt_payload(input: &[u8], packet: &MQTTPacket) -> IResult<&[u8], Vec<u8>> {
    take(packet.remaining_length)(input)
}

fn parse_complete_mqtt_packet(input: &[u8]) -> IResult<&[u8], MQTTPacket> {
    let (input, mut packet) = parse_mqtt_fixed_header(input)?;
    let (input, payload) = parse_mqtt_payload(input, &packet)?;
    
    Ok((input, MQTTPacket {
        payload: Some(payload.to_vec()),
        ..packet
    }))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_complete_mqtt_packet(&buffer) {
        Ok((_, packet)) => {
            println!("Parsed MQTT Packet: {:?}", packet);
            Ok(())
        },
        Err(e) => {
            eprintln!("Failed to parse MQTT packet: {:?}", e);
            std::process::exit(1);
        }
    }
}