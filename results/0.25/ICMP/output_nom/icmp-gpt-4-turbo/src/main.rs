use nom::{
    bytes::complete::take,
    number::complete::{be_u8, be_u16},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct IcmpHeader {
    icmp_type: u8,
    code: u8,
    checksum: u16,
    rest_of_header: u32,
}

#[derive(Debug)]
enum IcmpMessage {
    EchoReply {
        identifier: u16,
        sequence_number: u16,
        data: Vec<u8>,
    },
    DestinationUnreachable {
        unused: u32,
        data: Vec<u8>,
    },
    // Add other ICMP types as needed
}

fn parse_icmp_header(input: &[u8]) -> IResult<&[u8], IcmpHeader> {
    let (input, icmp_type) = be_u8(input)?;
    let (input, code) = be_u8(input)?;
    let (input, checksum) = be_u16(input)?;
    let (input, rest_of_header) = be_u32(input)?;
    Ok((
        input,
        IcmpHeader {
            icmp_type,
            code,
            checksum,
            rest_of_header,
        },
    ))
}

fn parse_icmp_message(input: &[u8], icmp_type: u8) -> IResult<&[u8], IcmpMessage> {
    match icmp_type {
        0 => { // Echo Reply
            let (input, identifier) = be_u16(input)?;
            let (input, sequence_number) = be_u16(input)?;
            Ok((input, IcmpMessage::EchoReply {
                identifier,
                sequence_number,
                data: input.to_vec(),
            }))
        },
        3 => { // Destination Unreachable
            let (input, unused) = be_u32(input)?;
            Ok((input, IcmpMessage::DestinationUnreachable {
                unused,
                data: input.to_vec(),
            }))
        },
        _ => Err(nom::Err::Error((input, nom::error::ErrorKind::Tag))),
    }
}

fn read_file(path: &str) -> io::Result<Vec<u8>> {
    let mut file = File::open(path)?;
    let mut data = Vec::new();
    file.read_to_end(&mut data)?;
    Ok(data)
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <file>", args[0]);
        return Ok(());
    }

    let data = read_file(&args[1])?;
    match parse_icmp_header(&data) {
        Ok((input, header)) => {
            println!("Parsed Header: {:?}", header);
            match parse_icmp_message(input, header.icmp_type) {
                Ok((_, message)) => {
                    println!("Parsed Message: {:?}", message);
                },
                Err(e) => println!("Failed to parse ICMP message: {:?}", e),
            }
        },
        Err(e) => println!("Failed to parse ICMP header: {:?}", e),
    }

    Ok(())
}