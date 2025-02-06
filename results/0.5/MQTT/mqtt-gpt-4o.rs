use nom::{
    bytes::complete::take,
    combinator::map_res,
    number::complete::be_u8,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
enum ControlPacketType {
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
    Unknown(u8),
}

#[derive(Debug)]
struct FixedHeader {
    packet_type: ControlPacketType,
    flags: u8,
    remaining_length: u32,
}

fn parse_control_packet_type(input: &[u8]) -> IResult<&[u8], ControlPacketType> {
    map_res(be_u8, |byte| {
        Ok::<ControlPacketType, nom::error::Error<&[u8]>>(match byte >> 4 {
            1 => ControlPacketType::Connect,
            2 => ControlPacketType::Connack,
            3 => ControlPacketType::Publish,
            4 => ControlPacketType::Puback,
            5 => ControlPacketType::Pubrec,
            6 => ControlPacketType::Pubrel,
            7 => ControlPacketType::Pubcomp,
            8 => ControlPacketType::Subscribe,
            9 => ControlPacketType::Suback,
            10 => ControlPacketType::Unsubscribe,
            11 => ControlPacketType::Unsuback,
            12 => ControlPacketType::Pingreq,
            13 => ControlPacketType::Pingresp,
            14 => ControlPacketType::Disconnect,
            15 => ControlPacketType::Auth,
            other => ControlPacketType::Unknown(other),
        })
    })(input)
}

fn parse_remaining_length(input: &[u8]) -> IResult<&[u8], u32> {
    let mut multiplier = 1;
    let mut value = 0;
    let mut i = 0;

    while i < input.len() {
        let encoded_byte = input[i];
        value += ((encoded_byte & 127) as u32) * multiplier;
        if multiplier > 128 * 128 * 128 {
            return Err(nom::Err::Failure(nom::error::Error::new(input, nom::error::ErrorKind::TooLarge)));
        }
        multiplier *= 128;
        i += 1;
        if (encoded_byte & 128) == 0 {
            break;
        }
    }
    Ok((&input[i..], value))
}

fn parse_fixed_header(input: &[u8]) -> IResult<&[u8], FixedHeader> {
    let (input, packet_type) = parse_control_packet_type(input)?;
    let (input, flags) = be_u8(input)?;
    let (input, remaining_length) = parse_remaining_length(input)?;

    Ok((
        input,
        FixedHeader {
            packet_type,
            flags,
            remaining_length,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_fixed_header(&buffer) {
        Ok((_, header)) => println!("{:?}", header),
        Err(e) => eprintln!("Failed to parse MQTT packet: {:?}", e),
    }

    Ok(())
}