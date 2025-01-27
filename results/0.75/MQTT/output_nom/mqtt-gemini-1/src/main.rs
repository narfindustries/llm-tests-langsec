use nom::{
    bytes::complete::{tag, take_while, take_while1},
    combinator::{map, map_res, opt, rest},
    number::complete::be_u16,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct MqttPacket {
    fixed_header: FixedHeader,
    remaining_length: u32,
    payload: Vec<u8>,
}

#[derive(Debug)]
struct FixedHeader {
    message_type: u8,
    flags: u8,
    qos: u8,
    retain: bool,
}


fn parse_remaining_length(input: &[u8]) -> IResult<&[u8], u32> {
    let mut multiplier: u32 = 1;
    let mut value: u32 = 0;
    let mut i = 0;
    loop {
        let byte = input.get(i).ok_or(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Eof)))?;
        let byte = *byte as u32;
        value += (byte & 0x7F) * multiplier;
        multiplier *= 128;
        if byte < 128 {
            break;
        }
        i += 1;
    }

    Ok((input.get(i + 1..).unwrap_or(&[]), value))

}

fn parse_fixed_header(input: &[u8]) -> IResult<&[u8], FixedHeader> {
    let (input, message_type) = input.get(0..1).ok_or(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Eof)))?;
    let message_type = message_type[0];

    let (input, flags) = input.get(1..2).ok_or(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Eof)))?;
    let flags = flags[0];

    let qos = (flags >> 1) & 0x03;
    let retain = (flags >> 3) & 0x01 != 0;


    Ok((input, FixedHeader { message_type, flags, qos: qos as u8, retain }))
}


fn parse_mqtt_packet(input: &[u8]) -> IResult<&[u8], MqttPacket> {
    let (input, fixed_header) = parse_fixed_header(input)?;
    let (input, remaining_length) = parse_remaining_length(input)?;
    let (input, payload) = take_while(|b| b != 0)(input)?;

    Ok((input, MqttPacket { fixed_header, remaining_length, payload: payload.to_vec() }))
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = match File::open(filename) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Error opening file: {}", err);
            return;
        }
    };

    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Ok(_) => (),
        Err(err) => {
            eprintln!("Error reading file: {}", err);
            return;
        }
    };

    match parse_mqtt_packet(&buffer) {
        Ok((_, packet)) => println!("{:?}", packet),
        Err(err) => eprintln!("Error parsing MQTT packet: {:?}", err),
    }
}
