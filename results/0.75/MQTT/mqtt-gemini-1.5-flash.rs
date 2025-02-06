use nom::{
    bytes::complete::{take},
    combinator::{map, opt},
    number::complete::be_u16,
    IResult,
    error::ErrorKind,
    multi::length_count,
};
use std::env;
use std::fs::read;
use num_traits::FromPrimitive;

#[derive(Debug)]
struct MqttProperty {
    id: u8,
    value: Vec<u8>,
}

#[derive(Debug)]
struct MqttPacket {
    packet_type: u8,
    flags: u8,
    remaining_length: u32,
    packet_identifier: Option<u16>,
    topic_name: Option<String>,
    payload: Option<Vec<u8>>,
    properties: Option<Vec<MqttProperty>>,
}

fn parse_remaining_length(input: &[u8]) -> IResult<&[u8], u32> {
    let mut len = 0;
    let mut multiplier = 1;
    let mut i = 0;
    loop {
        let byte = input[i];
        len += (byte & 0x7F) as u32 * multiplier;
        multiplier *= 128;
        i += 1;
        if byte < 128 {
            break;
        }
        if i >= input.len() {
            return Err(nom::Err::Incomplete(nom::Needed::Size(1)));
        }
    }
    Ok((&input[i..], len))
}


fn parse_mqtt_property(input: &[u8]) -> IResult<&[u8], MqttProperty> {
    let (input, id) = take(1usize)(input)?;
    let (input, len) = be_u16(input)?;
    let (input, value) = take(len as usize)(input)?;
    Ok((input, MqttProperty { id: id[0], value: value.to_vec() }))
}

fn parse_mqtt_packet(input: &[u8]) -> IResult<&[u8], MqttPacket> {
    let (input, packet_type) = take(1usize)(input)?;
    let (input, flags) = take(1usize)(input)?;
    let (input, remaining_length) = parse_remaining_length(input)?;
    let packet_type = packet_type[0];

    let mut remaining_input = input;
    let mut packet_identifier = None;
    let mut topic_name = None;
    let mut payload = None;
    let mut properties = None;

    if [3, 4, 8, 10, 11, 12, 13].contains(&packet_type) {
        let (rem, pid) = be_u16(remaining_input)?;
        packet_identifier = Some(pid);
        remaining_input = rem;
    }

    if packet_type == 2 {
        let (rem, topic_len) = be_u16(remaining_input)?;
        let (rem, topic) = take(topic_len as usize)(rem)?;
        topic_name = Some(String::from_utf8_lossy(topic).to_string());
        remaining_input = rem;
        let payload_len = remaining_length as usize - 2 - topic_len as usize;
        if payload_len > 0 {
            payload = Some(remaining_input[..payload_len].to_vec());
            remaining_input = &remaining_input[payload_len..];
        }
    }

    let (rem, property_len_bytes) = opt(be_u16)(remaining_input)?;
    let property_length = property_len_bytes.unwrap_or(0);

    if property_length > 0 {
        let (rem, parsed_properties) = length_count(property_length as usize, parse_mqtt_property)(remaining_input)?;
        properties = Some(parsed_properties);
        remaining_input = rem;
    }

    Ok((
        &[],
        MqttPacket {
            packet_type,
            flags: flags[0],
            remaining_length,
            packet_identifier,
            topic_name,
            payload,
            properties,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let data = match read(filename) {
        Ok(d) => d,
        Err(e) => {
            println!("Error reading file: {}", e);
            return;
        }
    };

    match parse_mqtt_packet(&data) {
        Ok((_, packet)) => println!("Parsed packet: {:?}", packet),
        Err(e) => println!("Error parsing packet: {:?}", e),
    }
}