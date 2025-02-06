extern crate nom;

use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u16, be_u8},
    sequence::tuple,
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::env;

#[derive(Debug)]
enum PacketType {
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
    Auth,
    Unknown,
}

impl From<u8> for PacketType {
    fn from(byte: u8) -> Self {
        match byte >> 4 {
            1 => PacketType::Connect,
            2 => PacketType::Connack,
            3 => PacketType::Publish,
            4 => PacketType::Puback,
            5 => PacketType::Pubrec,
            6 => PacketType::Pubrel,
            7 => PacketType::Pubcomp,
            8 => PacketType::Subscribe,
            9 => PacketType::Suback,
            10 => PacketType::Unsubscribe,
            11 => PacketType::Unsuback,
            12 => PacketType::Pingreq,
            13 => PacketType::Pingresp,
            14 => PacketType::Disconnect,
            15 => PacketType::Auth,
            _ => PacketType::Unknown,
        }
    }
}

#[derive(Debug)]
struct FixedHeader {
    packet_type: PacketType,
    flags: u8,
    remaining_length: usize,
}

fn variable_length(input: &[u8]) -> IResult<&[u8], usize> {
    let mut multiplier = 1;
    let mut value = 0;
    let mut index = 0;

    while index < input.len() {
        let encoded_byte = input[index];
        index += 1;
        value += ((encoded_byte & 127) as usize) * multiplier;
        multiplier *= 128;
        if multiplier > 128 * 128 * 128 {
            return Err(nom::Err::Failure(nom::error::Error::new(input, nom::error::ErrorKind::TooLarge)));
        }
        if encoded_byte & 128 == 0 {
            break;
        }
    }
    Ok((&input[index..], value))
}

fn parse_fixed_header(input: &[u8]) -> IResult<&[u8], FixedHeader> {
    let (input, byte1) = be_u8(input)?;
    let (input, remaining_length) = variable_length(input)?;

    Ok((
        input,
        FixedHeader {
            packet_type: PacketType::from(byte1),
            flags: byte1 & 0x0F,
            remaining_length,
        },
    ))
}

fn parse_connect(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, (_, protocol_version, connect_flags, keep_alive)) =
        tuple((tag(b"\x00\x04MQTT"), be_u8, be_u8, be_u16))(input)?;
    println!(
        "Connect: protocol_version={}, connect_flags={}, keep_alive={}",
        protocol_version, connect_flags, keep_alive
    );
    Ok((input, ()))
}

fn parse_mqtt_packet(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, fixed_header) = parse_fixed_header(input)?;
    println!("{:?}", fixed_header);

    match fixed_header.packet_type {
        PacketType::Connect => {
            let (input, _) = take(fixed_header.remaining_length)(input)?;
            parse_connect(input)
        }
        _ => {
            let (input, _) = take(fixed_header.remaining_length)(input)?;
            Ok((input, ()))
        }
    }
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: mqtt_parser <file>");
        return Ok(());
    }

    let filename = &args[1];
    let mut file = File::open(filename)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_mqtt_packet(&buffer) {
        Ok((_, _)) => println!("Parsing successful"),
        Err(e) => eprintln!("Error parsing file: {:?}", e),
    }

    Ok(())
}