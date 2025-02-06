use nom::{
    IResult,
    bytes::complete::{take_while},
    number::complete::{be_u8},
    error::ParseError,
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
}

#[derive(Debug)]
struct MqttPacket {
    packet_type: PacketType,
    flags: u8,
    remaining_length: usize,
    // Note: Include each specific packet's data structures here
}

fn parse_remaining_length(input: &[u8]) -> IResult<&[u8], usize> {
    let mut multiplier = 1;
    let mut value = 0usize;
    let mut i = 0;
    while let Some(&byte) = input.get(i) {
        value += ((byte & 0x7F) as usize) * multiplier;
        multiplier *= 128;
        i += 1;
        if byte & 0x80 == 0 {
            break;
        }
    }
    Ok((&input[i..], value))
}

fn parse_fixed_header(input: &[u8]) -> IResult<&[u8], (PacketType, u8, usize)> {
    let (input, packet_type_flag) = be_u8(input)?;
    let packet_type_code = (packet_type_flag >> 4) & 0x0F;
    let flags = packet_type_flag & 0x0F;
    let packet_type = match packet_type_code {
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
        _ => return Err(nom::Err::Failure(ParseError::from_error_kind(input, nom::error::ErrorKind::Switch))),
    };

    let (input, remaining_length) = parse_remaining_length(input)?;

    Ok((input, (packet_type, flags, remaining_length)))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("Usage: {} <mqtt_binary_input_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_fixed_header(&buffer) {
        Ok((_rest, header)) => {
            println!("{:?}", header);
        }
        Err(e) => {
            eprintln!("Failed to parse: {:?}", e);
        }
    }

    Ok(())
}