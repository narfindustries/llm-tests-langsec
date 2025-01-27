use nom::{
    bytes::complete::take,
    combinator::map,
    number::complete::{be_u16, be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct IcmpHeader {
    type_: u8,
    code: u8,
    checksum: u16,
    rest_of_header: u32,
}

#[derive(Debug)]
struct IcmpEchoRequest {
    identifier: u16,
    sequence_number: u16,
}

#[derive(Debug)]
struct IcmpRedirect {
    gateway_internet_address: u32,
}

#[derive(Debug)]
struct IcmpParameterProblem {
    pointer: u8,
    unused: [u8; 3],
}

#[derive(Debug)]
struct IcmpTimestamp {
    identifier: u16,
    sequence_number: u16,
    originate_timestamp: u32,
    receive_timestamp: u32,
    transmit_timestamp: u32,
}

fn parse_icmp_header(input: &[u8]) -> IResult<&[u8], IcmpHeader> {
    map(
        tuple((be_u8, be_u8, be_u16, be_u32)),
        |(type_, code, checksum, rest_of_header)| IcmpHeader {
            type_,
            code,
            checksum,
            rest_of_header,
        },
    )(input)
}

fn parse_echo_request(input: &[u8]) -> IResult<&[u8], IcmpEchoRequest> {
    map(tuple((be_u16, be_u16)), |(identifier, sequence_number)| {
        IcmpEchoRequest {
            identifier,
            sequence_number,
        }
    })(input)
}

fn parse_redirect(input: &[u8]) -> IResult<&[u8], IcmpRedirect> {
    map(be_u32, |gateway_internet_address| IcmpRedirect {
        gateway_internet_address,
    })(input)
}

fn parse_parameter_problem(input: &[u8]) -> IResult<&[u8], IcmpParameterProblem> {
    map(tuple((be_u8, take(3usize))), |(pointer, unused)| {
        IcmpParameterProblem {
            pointer,
            unused: [unused[0], unused[1], unused[2]],
        }
    })(input)
}

fn parse_timestamp(input: &[u8]) -> IResult<&[u8], IcmpTimestamp> {
    map(
        tuple((be_u16, be_u16, be_u32, be_u32, be_u32)),
        |(identifier, sequence_number, originate_timestamp, receive_timestamp, transmit_timestamp)| {
            IcmpTimestamp {
                identifier,
                sequence_number,
                originate_timestamp,
                receive_timestamp,
                transmit_timestamp,
            }
        },
    )(input)
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_icmp_header(&buffer) {
        Ok((remaining, header)) => {
            println!("ICMP Header: {:?}", header);
            
            match header.type_ {
                0 | 8 => {
                    if let Ok((_, echo)) = parse_echo_request(remaining) {
                        println!("Echo Request/Reply: {:?}", echo);
                    }
                }
                5 => {
                    if let Ok((_, redirect)) = parse_redirect(remaining) {
                        println!("Redirect: {:?}", redirect);
                    }
                }
                12 => {
                    if let Ok((_, param_prob)) = parse_parameter_problem(remaining) {
                        println!("Parameter Problem: {:?}", param_prob);
                    }
                }
                13 | 14 => {
                    if let Ok((_, timestamp)) = parse_timestamp(remaining) {
                        println!("Timestamp: {:?}", timestamp);
                    }
                }
                _ => println!("Unhandled ICMP type: {}", header.type_),
            }
        }
        Err(e) => eprintln!("Failed to parse ICMP header: {:?}", e),
    }

    Ok(())
}