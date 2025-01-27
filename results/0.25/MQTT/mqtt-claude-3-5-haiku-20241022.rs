use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{count, many0},
    number::complete::{be_u8, be_u16, be_u32},
    sequence::{tuple, preceded},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
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
}

#[derive(Debug)]
struct MQTTPacket {
    packet_type: MQTTPacketType,
    flags: u8,
    remaining_length: usize,
    payload: Option<Vec<u8>>,
}

fn parse_mqtt_packet(input: &[u8]) -> IResult<&[u8], MQTTPacket> {
    let (input, first_byte) = be_u8(input)?;
    let packet_type = match first_byte >> 4 {
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
        _ => return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
    };
    let flags = first_byte & 0x0F;

    let (input, remaining_length) = parse_remaining_length(input)?;

    let (input, payload) = if remaining_length > 0 {
        let (input, payload_bytes) = take(remaining_length)(input)?;
        (input, Some(payload_bytes.to_vec()))
    } else {
        (input, None)
    };

    Ok((input, MQTTPacket {
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

    loop {
        let (new_input, encoded_byte) = be_u8(input)?;
        input = new_input;
        value += (encoded_byte & 0x7F) as usize * multiplier;
        multiplier *= 128;
        bytes_read += 1;

        if encoded_byte & 0x80 == 0 {
            break;
        }

        if bytes_read > 4 {
            return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::TooLarge)));
        }
    }

    Ok((input, value))
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