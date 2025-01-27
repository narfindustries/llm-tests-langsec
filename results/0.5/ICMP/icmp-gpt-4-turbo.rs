use nom::{
    IResult, 
    bytes::complete::{take},
    number::complete::{be_u8, be_u16},
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct ICMPHeader {
    icmp_type: u8,
    code: u8,
    checksum: u16,
    rest_of_header: u32,
}

#[derive(Debug)]
struct ICMPMessage {
    header: ICMPHeader,
    payload: Vec<u8>,
}

fn parse_icmp_header(input: &[u8]) -> IResult<&[u8], ICMPHeader> {
    let (input, icmp_type) = be_u8(input)?;
    let (input, code) = be_u8(input)?;
    let (input, checksum) = be_u16(input)?;
    let (input, rest_of_header) = be_u32(input)?;
    Ok((input, ICMPHeader {
        icmp_type,
        code,
        checksum,
        rest_of_header,
    }))
}

fn parse_icmp_message(input: &[u8]) -> IResult<&[u8], ICMPMessage> {
    let (input, header) = parse_icmp_header(input)?;
    let (remaining, payload) = take(input.len())(input)?;
    Ok((remaining, ICMPMessage {
        header,
        payload: payload.to_vec(),
    }))
}

fn read_file_contents(filename: &str) -> io::Result<Vec<u8>> {
    let mut file = File::open(filename)?;
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

    let filename = &args[1];
    let data = read_file_contents(filename)?;
    match parse_icmp_message(&data) {
        Ok((_, icmp_message)) => {
            println!("{:?}", icmp_message);
        },
        Err(e) => {
            println!("Failed to parse ICMP message: {:?}", e);
        }
    }

    Ok(())
}