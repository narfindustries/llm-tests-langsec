use nom::{
    bytes::complete::{tag, take_while, take_while1},
    combinator::{map, map_res, opt},
    number::complete::{be_u16, be_u32, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct MqttPacket {
    fixed_header: FixedHeader,
    variable_header: Option<VariableHeader>,
    payload: Option<Vec<u8>>,
}

#[derive(Debug)]
struct FixedHeader {
    message_type: u8,
    flags: u8,
    remaining_length: u32,
}

#[derive(Debug)]
struct VariableHeader {
    // Add variable header fields as needed based on the MQTT specification
    // Example:  packet_identifier: u16,
}


fn read_remaining_length(input: &[u8]) -> IResult<&[u8], u32> {
    let mut multiplier: u32 = 1;
    let mut value: u32 = 0;
    let mut rest = input;

    loop {
        let (r, byte) = be_u8(rest)?;
        value += (byte & 0x7F) as u32 * multiplier;
        multiplier *= 128;
        rest = r;
        if byte < 0x80 {
            break;
        }
    }
    Ok((rest, value))
}

fn parse_fixed_header(input: &[u8]) -> IResult<&[u8], FixedHeader> {
    let (rest, (message_type, flags, remaining_length)) = tuple((be_u8, be_u8, read_remaining_length))(input)?;
    Ok((rest, FixedHeader { message_type, flags, remaining_length }))
}


fn parse_variable_header(input: &[u8], message_type: u8) -> IResult<&[u8], Option<VariableHeader>> {
    // Implement parsing of variable header based on message type
    // This is a placeholder, you need to add the logic for each message type
    match message_type {
        //Example for PUBLISH
        3 => {
            let (rest, (topic_len, topic, packet_id)) = tuple((be_u16, take_while1(|c| c != 0), opt(be_u16)))(input)?;
            let topic_str = std::str::from_utf8(topic).map_err(|e| nom::Err::Error(nom::error::Error::new(input, e)))?;
            Ok((rest, Some(VariableHeader{packet_id: packet_id.unwrap_or(0)})))
        },
        _ => Ok((input, None)),
    }
}

fn parse_payload(input: &[u8], remaining_length: u32) -> IResult<&[u8], Option<Vec<u8>>> {
    if remaining_length > 0 {
        let (rest, payload) = take_while(|_| true)(input)?;
        Ok((rest, Some(payload.to_vec())))
    } else {
        Ok((input, None))
    }
}

fn parse_mqtt_packet(input: &[u8]) -> IResult<&[u8], MqttPacket> {
    let (rest, fixed_header) = parse_fixed_header(input)?;
    let (rest, variable_header) = parse_variable_header(rest, fixed_header.message_type)?;
    let (rest, payload) = parse_payload(rest, fixed_header.remaining_length)?;
    Ok((rest, MqttPacket { fixed_header, variable_header, payload }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_mqtt_packet(&buffer) {
        Ok((_, packet)) => println!("{:?}", packet),
        Err(e) => eprintln!("Error parsing MQTT packet: {:?}", e),
    }
}
