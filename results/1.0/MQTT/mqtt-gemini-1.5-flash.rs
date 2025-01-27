use nom::{
    bytes::complete::{tag, take_while, take_until},
    combinator::{map, map_res, opt},
    number::complete::be_u32,
    sequence::{preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct MqttPacket {
    fixed_header: FixedHeader,
    // Add other fields as needed based on the variable header and payload
}


#[derive(Debug)]
struct FixedHeader {
    packet_type: u8,
    flags: u8,
    remaining_length: u32,
}

fn parse_remaining_length(input: &[u8]) -> IResult<&[u8], u32> {
    let mut remaining_length: u32 = 0;
    let mut multiplier: u32 = 1;
    let mut i = 0;

    loop {
        let byte = input[i];
        remaining_length += ((byte & 0x7F) as u32) * multiplier;
        multiplier *= 128;
        if byte & 0x80 == 0 {
            break;
        }
        i += 1;
        if i >= input.len() {
            return Err(nom::Err::Incomplete(nom::Needed::Size(i + 1)));
        }
    }
    Ok((&input[i + 1..], remaining_length))

}

fn parse_fixed_header(input: &[u8]) -> IResult<&[u8], FixedHeader> {
    let (input, packet_type) = take_while(|b| b != &0x00)(input)?;
    let (input, flags) = take_while(|b| b != &0x00)(input)?;
    let (input, remaining_length) = parse_remaining_length(input)?;


    Ok((
        input,
        FixedHeader {
            packet_type: packet_type[0],
            flags: flags[0],
            remaining_length,
        },
    ))
}


fn parse_mqtt_packet(input: &[u8]) -> IResult<&[u8], MqttPacket> {
    let (input, fixed_header) = parse_fixed_header(input)?;

    // Parse variable header and payload based on packet type
    // This is a placeholder, you'll need to implement the specific parsers for each packet type

    Ok((input, MqttPacket { fixed_header }))
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
            eprintln!("Failed to open file {}: {}", filename, err);
            return;
        }
    };

    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Ok(_) => (),
        Err(err) => {
            eprintln!("Failed to read file: {}", err);
            return;
        }
    };

    match parse_mqtt_packet(&buffer) {
        Ok((_, packet)) => println!("Parsed MQTT packet: {:?}", packet),
        Err(err) => eprintln!("Failed to parse MQTT packet: {:?}", err),
    }
}
