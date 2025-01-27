extern crate nom;
use nom::{
    bits::complete::{tag, take},
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
    typ: u8,
    code: u8,
    checksum: u16,
    rest_of_header: u32,
}

#[derive(Debug)]
struct IcmpEchoReply {
    identifier: u16,
    sequence_number: u16,
}

#[derive(Debug)]
struct IcmpDestUnreachable {
    unused: u32,
}

#[derive(Debug)]
struct IcmpTimeExceeded {
    unused: u32,
}

#[derive(Debug)]
struct IcmpParameterProblem {
    pointer: u8,
    unused: [u8; 3],
}

#[derive(Debug)]
struct IcmpRedirect {
    gateway_internet_address: u32,
}

#[derive(Debug)]
struct IcmpEchoRequest {
    identifier: u16,
    sequence_number: u16,
}

fn parse_icmp_header(input: &[u8]) -> IResult<&[u8], IcmpHeader> {
    let (input, (typ, code, checksum, rest_of_header)) =
        tuple((be_u8, be_u8, be_u16, be_u32))(input)?;

    Ok((
        input,
        IcmpHeader {
            typ,
            code,
            checksum,
            rest_of_header,
        },
    ))
}

fn parse_echo_reply(input: &[u8]) -> IResult<&[u8], IcmpEchoReply> {
    let (input, (identifier, sequence_number)) = tuple((be_u16, be_u16))(input)?;

    Ok((
        input,
        IcmpEchoReply {
            identifier,
            sequence_number,
        },
    ))
}

fn parse_dest_unreachable(input: &[u8]) -> IResult<&[u8], IcmpDestUnreachable> {
    let (input, unused) = be_u32(input)?;

    Ok((input, IcmpDestUnreachable { unused }))
}

fn parse_time_exceeded(input: &[u8]) -> IResult<&[u8], IcmpTimeExceeded> {
    let (input, unused) = be_u32(input)?;

    Ok((input, IcmpTimeExceeded { unused }))
}

fn parse_parameter_problem(input: &[u8]) -> IResult<&[u8], IcmpParameterProblem> {
    let (input, (pointer, unused1, unused2, unused3)) =
        tuple((be_u8, be_u8, be_u8, be_u8))(input)?;

    Ok((
        input,
        IcmpParameterProblem {
            pointer,
            unused: [unused1, unused2, unused3],
        },
    ))
}

fn parse_redirect(input: &[u8]) -> IResult<&[u8], IcmpRedirect> {
    let (input, gateway_internet_address) = be_u32(input)?;

    Ok((
        input,
        IcmpRedirect {
            gateway_internet_address,
        },
    ))
}

fn parse_echo_request(input: &[u8]) -> IResult<&[u8], IcmpEchoRequest> {
    let (input, (identifier, sequence_number)) = tuple((be_u16, be_u16))(input)?;

    Ok((
        input,
        IcmpEchoRequest {
            identifier,
            sequence_number,
        },
    ))
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
            
            match header.typ {
                0 => {
                    if let Ok((_, echo_reply)) = parse_echo_reply(remaining) {
                        println!("Echo Reply: {:?}", echo_reply);
                    }
                }
                3 => {
                    if let Ok((_, dest_unreachable)) = parse_dest_unreachable(remaining) {
                        println!("Destination Unreachable: {:?}", dest_unreachable);
                    }
                }
                4 => {
                    if let Ok((_, source_quench)) = parse_dest_unreachable(remaining) {
                        println!("Source Quench: {:?}", source_quench);
                    }
                }
                5 => {
                    if let Ok((_, redirect)) = parse_redirect(remaining) {
                        println!("Redirect: {:?}", redirect);
                    }
                }
                8 => {
                    if let Ok((_, echo_request)) = parse_echo_request(remaining) {
                        println!("Echo Request: {:?}", echo_request);
                    }
                }
                11 => {
                    if let Ok((_, time_exceeded)) = parse_time_exceeded(remaining) {
                        println!("Time Exceeded: {:?}", time_exceeded);
                    }
                }
                12 => {
                    if let Ok((_, parameter_problem)) = parse_parameter_problem(remaining) {
                        println!("Parameter Problem: {:?}", parameter_problem);
                    }
                }
                _ => println!("Unknown ICMP type"),
            }
        }
        Err(e) => println!("Error parsing ICMP header: {:?}", e),
    }

    Ok(())
}