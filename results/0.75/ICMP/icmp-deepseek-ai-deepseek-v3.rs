use nom::{
    bytes::complete::take,
    number::complete::{be_u16, be_u32, be_u8},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{self, Read},
};

#[derive(Debug)]
struct IcmpHeader {
    type_field: u8,
    code: u8,
    checksum: u16,
    rest_of_header: Vec<u8>,
    data: Vec<u8>,
}

fn parse_icmp(input: &[u8]) -> IResult<&[u8], IcmpHeader> {
    let (input, type_field) = be_u8(input)?;
    let (input, code) = be_u8(input)?;
    let (input, checksum) = be_u16(input)?;
    let (input, rest_of_header) = match type_field {
        0 | 8 => {
            let (input, identifier) = take(2usize)(input)?;
            let (input, sequence_number) = take(2usize)(input)?;
            let mut rest = Vec::new();
            rest.extend_from_slice(identifier);
            rest.extend_from_slice(sequence_number);
            (input, rest)
        }
        3 | 11 => {
            let (input, unused) = take(4usize)(input)?;
            let (input, original_datagram) = take(8usize)(input)?;
            let mut rest = Vec::new();
            rest.extend_from_slice(unused);
            rest.extend_from_slice(original_datagram);
            (input, rest)
        }
        5 => {
            let (input, gateway_address) = take(4usize)(input)?;
            (input, gateway_address.to_vec())
        }
        _ => (input, Vec::new()),
    };
    let (input, data) = take(input.len())(input)?;
    Ok((
        input,
        IcmpHeader {
            type_field,
            code,
            checksum,
            rest_of_header,
            data: data.to_vec(),
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        std::process::exit(1);
    }
    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;
    match parse_icmp(&buffer) {
        Ok((_, icmp)) => println!("{:#?}", icmp),
        Err(e) => eprintln!("Failed to parse ICMP: {:?}", e),
    }
    Ok(())
}