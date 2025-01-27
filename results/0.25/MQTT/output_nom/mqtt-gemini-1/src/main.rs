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
    // Add variable header fields as needed based on the MQTT message type
    // Example for CONNECT:
    protocol_name: String,
    protocol_level: u8,
    connect_flags: u8,
    keep_alive: u16,
    // ... other fields ...
}


fn read_remaining_length(input: &[u8]) -> IResult<&[u8], u32> {
    let mut multiplier: u32 = 1;
    let mut value: u32 = 0;
    let mut remaining_bytes = input;

    loop {
        let (input_rest, byte) = be_u8(remaining_bytes)?;
        remaining_bytes = input_rest;
        value += (byte & 0x7F) as u32 * multiplier;
        if byte & 0x80 == 0 {
            break;
        }
        multiplier *= 128;
        if multiplier > 0x2000000 {
            return Err(nom::Err::Failure(nom::error::Error::new(input, nom::error::ErrorKind::Length)));
        }
    }
    Ok((remaining_bytes, value))
}

fn parse_fixed_header(input: &[u8]) -> IResult<&[u8], FixedHeader> {
    let (input, message_type) = be_u8(input)?;
    let (input, flags) = be_u8(input)?;
    let (input, remaining_length) = read_remaining_length(input)?;
    Ok((
        input,
        FixedHeader {
            message_type,
            flags,
            remaining_length,
        },
    ))
}


// Implement parsing functions for variable headers and payloads based on message type
fn parse_connect_variable_header(input: &[u8]) -> IResult<&[u8], VariableHeader> {
    let (input, protocol_name) = map_res(take_while1(|c| c != 0), |bytes: &[u8]| {
        String::from_utf8(bytes.to_vec())
    })(input)?;
    let (input, protocol_level) = be_u8(input)?;
    let (input, connect_flags) = be_u8(input)?;
    let (input, keep_alive) = be_u16(input)?;
    Ok((
        input,
        VariableHeader {
            protocol_name,
            protocol_level,
            connect_flags,
            keep_alive,
        },
    ))
}

fn parse_mqtt_packet(input: &[u8]) -> IResult<&[u8], MqttPacket> {
    let (input, fixed_header) = parse_fixed_header(input)?;
    let (input, variable_header) = match fixed_header.message_type {
        1 => {
            let (input, vh) = parse_connect_variable_header(input)?;
            let (input, payload) = take_while(|_| true)(input)?;
            (input, Some(vh))
        }
        _ => (input, None),
    };

    let (input, payload) = if fixed_header.remaining_length > 0 {
        let (input, payload) = take_while(|_| true)(input)?;
        (input, Some(payload.to_vec()))
    } else {
        (input, None)
    };

    Ok((
        input,
        MqttPacket {
            fixed_header,
            variable_header,
            payload,
        },
    ))
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
        Err(e) => eprintln!("Parsing error: {:?}", e),
    }
}
