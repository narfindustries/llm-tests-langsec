use std::env;
use std::fs::File;
use std::io::Read;
use nom::{
    IResult,
    number::complete::{be_u8, be_u16, be_u32},
    bytes::complete::take,
};

#[derive(Debug)]
enum IcmpType {
    EchoReply(u8, u16, u16, Vec<u8>),       // Type 0
    DestinationUnreachable(u8, Vec<u8>),    // Type 3
    SourceQuench(u8, Vec<u8>),              // Type 4
    Redirect(u8, u32, Vec<u8>),             // Type 5
    EchoRequest(u8, u16, u16, Vec<u8>),     // Type 8
    TimeExceeded(u8, Vec<u8>),              // Type 11
    ParameterProblem(u8, u8, Vec<u8>),      // Type 12
    Timestamp(u8, u16, u16, u32, u32, u32), // Type 13
    TimestampReply(u8, u16, u16, u32, u32, u32), // Type 14
    Unknown(u8, Vec<u8>),                   // Any other type
}

#[derive(Debug)]
struct IcmpPacket {
    icmp_type: u8,
    code: u8,
    checksum: u16,
    rest_of_header: Vec<u8>,
    data: IcmpType,
}

fn parse_icmp(input: &[u8]) -> IResult<&[u8], IcmpPacket> {
    let (input, icmp_type) = be_u8(input)?;
    let (input, code) = be_u8(input)?;
    let (input, checksum) = be_u16(input)?;
    let rest_len: usize = input.len().min(4);
    let (input, rest_of_header) = take(rest_len)(input)?;

    let data = match icmp_type {
        0 => {
            let (data, identifier) = be_u16(rest_of_header)?;
            let (data, sequence_number) = be_u16(data)?;
            IcmpType::EchoReply(icmp_type, identifier, sequence_number, input.to_vec())
        }
        3 | 4 | 11 => IcmpType::DestinationUnreachable(icmp_type, input.to_vec()),
        5 => {
            let (data, gateway) = be_u32(rest_of_header)?;
            IcmpType::Redirect(icmp_type, gateway, data.to_vec())
        }
        8 => {
            let (data, identifier) = be_u16(rest_of_header)?;
            let (data, sequence_number) = be_u16(data)?;
            IcmpType::EchoRequest(icmp_type, identifier, sequence_number, input.to_vec())
        }
        12 => {
            let (data, pointer) = be_u8(rest_of_header)?;
            IcmpType::ParameterProblem(icmp_type, pointer, data.to_vec())
        }
        13 | 14 => {
            let (data, identifier) = be_u16(rest_of_header)?;
            let (data, sequence_number) = be_u16(data)?;
            let (data, originate_timestamp) = be_u32(data)?;
            let (data, receive_timestamp) = be_u32(data)?;
            let (data, transmit_timestamp) = be_u32(data)?;
            if icmp_type == 13 {
                IcmpType::Timestamp(icmp_type, identifier, sequence_number, originate_timestamp, receive_timestamp, transmit_timestamp)
            } else {
                IcmpType::TimestampReply(icmp_type, identifier, sequence_number, originate_timestamp, receive_timestamp, transmit_timestamp)
            }
        }
        _ => IcmpType::Unknown(icmp_type, input.to_vec()),
    };

    let packet = IcmpPacket {
        icmp_type,
        code,
        checksum,
        rest_of_header: rest_of_header.to_vec(),
        data,
    };

    Ok((input, packet))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_icmp(&buffer) {
        Ok((_, packet)) => println!("{:#?}", packet),
        Err(e) => eprintln!("Failed to parse ICMP packet: {:?}", e),
    }
}