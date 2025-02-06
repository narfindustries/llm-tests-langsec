use nom::{
    bytes::complete::take,
    error::{Error, ErrorKind},
    multi::length_data,
    number::complete::{be_u16, be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::{
    collections::HashMap,
    env,
    fs::File,
    io::{self, Read},
};

#[derive(Debug)]
struct MqttPacket {
    packet_type: u8,
    flags: u8,
    remaining_length: u32,
    variable_header: Vec<u8>,
    payload: Vec<u8>,
}

#[derive(Debug)]
struct ConnectPacket {
    protocol_name: String,
    protocol_level: u8,
    connect_flags: u8,
    keep_alive: u16,
    properties: HashMap<u8, Vec<u8>>,
    client_id: String,
    will_topic: Option<String>,
    will_payload: Option<Vec<u8>>,
    username: Option<String>,
    password: Option<Vec<u8>>,
}

fn parse_mqtt_packet(input: &[u8]) -> IResult<&[u8], MqttPacket> {
    let (input, (packet_type_flags, remaining_length)) = tuple((be_u8, parse_remaining_length))(input)?;
    let (input, variable_header_and_payload) = take(remaining_length)(input)?;
    let (variable_header, payload) = take_variable_header_and_payload(packet_type_flags >> 4, variable_header_and_payload);

    Ok((
        input,
        MqttPacket {
            packet_type: packet_type_flags >> 4,
            flags: packet_type_flags & 0x0F,
            remaining_length,
            variable_header: variable_header.to_vec(),
            payload: payload.to_vec(),
        },
    ))
}

fn parse_remaining_length(input: &[u8]) -> IResult<&[u8], u32> {
    let mut value = 0;
    let mut shift = 0;
    let mut input = input;

    loop {
        let (remaining, byte) = be_u8(input)?;
        value |= ((byte & 0x7F) as u32) << shift;
        shift += 7;
        input = remaining;

        if (byte & 0x80) == 0 {
            break;
        }
    }

    Ok((input, value))
}

fn take_variable_header_and_payload(packet_type: u8, input: &[u8]) -> (&[u8], &[u8]) {
    match packet_type {
        1 => parse_connect_variable_header(input),
        _ => (&[], input),
    }
}

fn parse_connect_variable_header(input: &[u8]) -> (&[u8], &[u8]) {
    let (input, _) = parse_string(input).unwrap();
    let (input, _) = be_u8(input).unwrap();
    let (input, _) = be_u8(input).unwrap();
    let (input, _) = be_u16(input).unwrap();
    let (input, _) = parse_properties(input).unwrap();

    let (input, _) = parse_string(input).unwrap();
    let (input, _) = if input.is_empty() {
        (input, None)
    } else {
        let (input, _) = parse_string(input).unwrap();
        (input, Some(()))
    };
    let (input, _) = if input.is_empty() {
        (input, None)
    } else {
        let (input, _) = length_data(be_u16)(input).unwrap();
        (input, Some(()))
    };
    let (input, _) = if input.is_empty() {
        (input, None)
    } else {
        let (input, _) = parse_string(input).unwrap();
        (input, Some(()))
    };
    let (input, _) = if input.is_empty() {
        (input, None)
    } else {
        let (input, _) = length_data(be_u16)(input).unwrap();
        (input, Some(()))
    };

    (&[], input)
}

fn parse_string(input: &[u8]) -> IResult<&[u8], String> {
    let (input, len) = be_u16(input)?;
    let (input, str_data) = take(len)(input)?;
    Ok((input, String::from_utf8_lossy(str_data).to_string()))
}

fn parse_properties(input: &[u8]) -> IResult<&[u8], HashMap<u8, Vec<u8>>> {
    let (input, properties_len) = be_u32(input)?;
    let (input, properties_data) = take(properties_len)(input)?;
    let mut properties = HashMap::new();
    let mut remaining = properties_data;

    while !remaining.is_empty() {
        let (rest, identifier) = be_u8(remaining)?;
        let (rest, value) = length_data(be_u16)(rest)?;
        properties.insert(identifier, value.to_vec());
        remaining = rest;
    }

    Ok((input, properties))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <binary file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_mqtt_packet(&buffer) {
        Ok((_, packet)) => println!("{:?}", packet),
        Err(e) => eprintln!("Failed to parse MQTT packet: {:?}", e),
    }

    Ok(())
}