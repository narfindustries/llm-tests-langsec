use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{count, length_count, length_data},
    number::complete::{be_u8, be_u16, be_u32},
    sequence::{tuple, preceded},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
enum MqttPacketType {
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

#[derive(Debug)]
struct MqttPacket {
    packet_type: MqttPacketType,
    flags: u8,
    remaining_length: usize,
    payload: Option<Vec<u8>>,
}

fn parse_mqtt_packet(input: &[u8]) -> IResult<&[u8], MqttPacket> {
    let (input, first_byte) = be_u8(input)?;
    let packet_type = match first_byte >> 4 {
        1 => MqttPacketType::Connect,
        2 => MqttPacketType::ConnAck,
        3 => MqttPacketType::Publish,
        4 => MqttPacketType::PubAck,
        5 => MqttPacketType::PubRec,
        6 => MqttPacketType::PubRel,
        7 => MqttPacketType::PubComp,
        8 => MqttPacketType::Subscribe,
        9 => MqttPacketType::SubAck,
        10 => MqttPacketType::Unsubscribe,
        11 => MqttPacketType::UnsubAck,
        12 => MqttPacketType::PingReq,
        13 => MqttPacketType::PingResp,
        14 => MqttPacketType::Disconnect,
        _ => return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
    };
    let flags = first_byte & 0x0F;

    let (input, remaining_length) = parse_remaining_length(input)?;

    let (input, payload) = if remaining_length > 0 {
        let (input, payload_data) = take(remaining_length)(input)?;
        (input, Some(payload_data.to_vec()))
    } else {
        (input, None)
    };

    Ok((input, MqttPacket {
        packet_type,
        flags,
        remaining_length,
        payload,
    }))
}

fn parse_remaining_length(input: &[u8]) -> IResult<&[u8], usize> {
    let mut multiplier = 1;
    let mut value = 0;
    let mut bytes_read = 0;

    for (index, &byte) in input.iter().enumerate() {
        value += (byte & 0x7F) as usize * multiplier;
        multiplier *= 128;
        bytes_read += 1;

        if byte & 0x80 == 0 {
            return Ok((&input[bytes_read..], value));
        }

        if index > 3 {
            return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::TooLarge)));
        }
    }

    Err(nom::Err::Incomplete(nom::Needed::new(1)))
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
        }
        Err(e) => {
            eprintln!("Failed to parse MQTT packet: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}