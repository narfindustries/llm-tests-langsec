use nom::{
    bytes::complete::take,
    combinator::{map, map_res},
    error::Error,
    number::complete::{be_u16, be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::{
    fs::File,
    io::Read,
};

#[derive(Debug)]
struct IcmpHeader {
    type_field: u8,
    code: u8,
    checksum: u16,
    variable_fields: u32,
}

#[derive(Debug)]
struct IcmpMessage {
    header: IcmpHeader,
    data: Vec<u8>,
}

fn parse_icmp_header(input: &[u8]) -> IResult<&[u8], IcmpHeader> {
    let (input, (type_field, code, checksum, variable_fields)) = tuple((be_u8, be_u8, be_u16, be_u32))(input)?;
    Ok((
        input,
        IcmpHeader {
            type_field,
            code,
            checksum,
            variable_fields,
        },
    ))
}

fn parse_icmp_message(input: &[u8]) -> IResult<&[u8], IcmpMessage> {
    let (input, header) = parse_icmp_header(input)?;
    let (input, data) = map(take(input.len()), Vec::from)(input)?;
    Ok((
        input,
        IcmpMessage {
            header,
            data,
        },
    ))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <binary file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_icmp_message(&buffer) {
        Ok((_, message)) => println!("{:#?}", message),
        Err(e) => eprintln!("Failed to parse ICMP message: {:?}", e),
    }
}