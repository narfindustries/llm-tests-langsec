use nom::{
    bytes::complete::take,
    combinator::map,
    error::{Error, ErrorKind},
    number::complete::be_u32,
    IResult,
};
use std::fs::read;
use std::process;

#[derive(Debug)]
enum MqttPacketType {
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
}

fn mqtt_packet_type(input: &[u8]) -> IResult<&[u8], MqttPacketType> {
    let (input, byte) = take(1usize)(input)?;
    let packet_type = match byte[0] {
        1 => MqttPacketType::Connect,
        2 => MqttPacketType::Connack,
        3 => MqttPacketType::Publish,
        4 => MqttPacketType::Puback,
        5 => MqttPacketType::Pubrec,
        6 => MqttPacketType::Pubrel,
        7 => MqttPacketType::Pubcomp,
        8 => MqttPacketType::Subscribe,
        9 => MqttPacketType::Suback,
        10 => MqttPacketType::Unsubscribe,
        11 => MqttPacketType::Unsuback,
        12 => MqttPacketType::Pingreq,
        13 => MqttPacketType::Pingresp,
        14 => MqttPacketType::Disconnect,
        _ => return Err(nom::Err::Error(Error::new(input, ErrorKind::Custom(1)))),
    };
    Ok((input, packet_type))
}

fn remaining_length(input: &[u8]) -> IResult<&[u8], u32> {
    let mut len = 0;
    let mut multiplier = 1;
    let mut i = 0;
    loop {
        let byte = input[i];
        len += (byte & 0x7F) as u32 * multiplier;
        multiplier *= 128;
        if byte < 128 {
            break;
        }
        i += 1;
    }
    Ok((&input[i + 1..], len))
}

fn parse_mqtt_packet(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, packet_type) = mqtt_packet_type(input)?;
    println!("Packet Type: {:?}", packet_type);
    let (input, remaining_len) = remaining_length(input)?;
    println!("Remaining Length: {}", remaining_len);
    let (input, _) = take(remaining_len as usize)(input)?;
    Ok((input, ()))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        process::exit(1);
    }

    let filename = &args[1];
    match read(filename) {
        Ok(buffer) => {
            match parse_mqtt_packet(&buffer) {
                Ok((_, _)) => println!("MQTT packet parsed successfully."),
                Err(e) => {
                    eprintln!("Error parsing MQTT packet: {:?}", e);
                    process::exit(1);
                }
            }
        }
        Err(e) => {
            eprintln!("Error reading file: {}", e);
            process::exit(1);
        }
    }
}

