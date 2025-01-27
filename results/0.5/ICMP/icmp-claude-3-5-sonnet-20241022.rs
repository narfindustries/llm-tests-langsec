use std::env;
use std::fs::File;
use std::io::Read;
use nom::{
    IResult,
    number::complete::{be_u8, be_u16, be_u32},
    bytes::complete::take,
    sequence::tuple,
    combinator::map,
};

#[derive(Debug)]
pub struct IcmpHeader {
    icmp_type: u8,
    code: u8,
    checksum: u16,
    rest_of_header: u32,
}

#[derive(Debug)]
pub struct IcmpEchoData {
    identifier: u16,
    sequence_number: u16,
}

#[derive(Debug)]
pub struct IcmpRedirectData {
    gateway_internet_address: u32,
}

#[derive(Debug)]
pub struct IcmpParameterProblemData {
    pointer: u8,
    unused: [u8; 3],
}

#[derive(Debug)]
pub struct IcmpTimestampData {
    identifier: u16,
    sequence_number: u16,
    originate_timestamp: u32,
    receive_timestamp: u32,
    transmit_timestamp: u32,
}

#[derive(Debug)]
pub struct IcmpInformationData {
    identifier: u16,
    sequence_number: u16,
}

fn parse_icmp_header(input: &[u8]) -> IResult<&[u8], IcmpHeader> {
    map(
        tuple((be_u8, be_u8, be_u16, be_u32)),
        |(icmp_type, code, checksum, rest_of_header)| IcmpHeader {
            icmp_type,
            code,
            checksum,
            rest_of_header,
        }
    )(input)
}

fn parse_echo_data(input: &[u8]) -> IResult<&[u8], IcmpEchoData> {
    map(
        tuple((be_u16, be_u16)),
        |(identifier, sequence_number)| IcmpEchoData {
            identifier,
            sequence_number,
        }
    )(input)
}

fn parse_redirect_data(input: &[u8]) -> IResult<&[u8], IcmpRedirectData> {
    map(
        be_u32,
        |gateway_internet_address| IcmpRedirectData {
            gateway_internet_address,
        }
    )(input)
}

fn parse_parameter_problem_data(input: &[u8]) -> IResult<&[u8], IcmpParameterProblemData> {
    map(
        tuple((be_u8, take(3usize))),
        |(pointer, unused)| IcmpParameterProblemData {
            pointer,
            unused: [unused[0], unused[1], unused[2]],
        }
    )(input)
}

fn parse_timestamp_data(input: &[u8]) -> IResult<&[u8], IcmpTimestampData> {
    map(
        tuple((be_u16, be_u16, be_u32, be_u32, be_u32)),
        |(identifier, sequence_number, originate_timestamp, receive_timestamp, transmit_timestamp)| 
        IcmpTimestampData {
            identifier,
            sequence_number,
            originate_timestamp,
            receive_timestamp,
            transmit_timestamp,
        }
    )(input)
}

fn parse_information_data(input: &[u8]) -> IResult<&[u8], IcmpInformationData> {
    map(
        tuple((be_u16, be_u16)),
        |(identifier, sequence_number)| IcmpInformationData {
            identifier,
            sequence_number,
        }
    )(input)
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_icmp_header(&buffer) {
        Ok((remaining, header)) => {
            println!("ICMP Header: {:?}", header);
            
            match header.icmp_type {
                0 | 8 => { // Echo Reply or Echo Request
                    if let Ok((_, echo_data)) = parse_echo_data(remaining) {
                        println!("Echo Data: {:?}", echo_data);
                    }
                },
                5 => { // Redirect
                    if let Ok((_, redirect_data)) = parse_redirect_data(remaining) {
                        println!("Redirect Data: {:?}", redirect_data);
                    }
                },
                12 => { // Parameter Problem
                    if let Ok((_, param_data)) = parse_parameter_problem_data(remaining) {
                        println!("Parameter Problem Data: {:?}", param_data);
                    }
                },
                13 | 14 => { // Timestamp or Timestamp Reply
                    if let Ok((_, timestamp_data)) = parse_timestamp_data(remaining) {
                        println!("Timestamp Data: {:?}", timestamp_data);
                    }
                },
                15 | 16 => { // Information Request or Information Reply
                    if let Ok((_, info_data)) = parse_information_data(remaining) {
                        println!("Information Data: {:?}", info_data);
                    }
                },
                _ => println!("Unknown or unhandled ICMP type"),
            }
        },
        Err(e) => eprintln!("Failed to parse ICMP header: {:?}", e),
    }

    Ok(())
}