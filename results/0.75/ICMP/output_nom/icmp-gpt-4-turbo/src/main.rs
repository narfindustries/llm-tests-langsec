use nom::{
    IResult, bytes::complete::take, number::complete::{be_u8, be_u16, be_u32},
    combinator::map_parser, branch::permutation
};
use std::fs::File;
use std::io::{self, Read};
use std::env;

#[derive(Debug)]
struct ICMPHeader {
    icmp_type: u8,
    code: u8,
    checksum: u16,
}

#[derive(Debug)]
enum ICMPMessage {
    EchoReply { id: u16, seq_number: u16 },
    DestinationUnreachable,
    SourceQuench,
    Redirect,
    EchoRequest { id: u16, seq_number: u16 },
    TimeExceeded,
    ParameterProblem,
    Timestamp { id: u16, seq_number: u16, originate_timestamp: u32, receive_timestamp: u32, transmit_timestamp: u32 },
    TimestampReply { id: u16, seq_number: u16, originate_timestamp: u32, receive_timestamp: u32, transmit_timestamp: u32 },
    InformationRequest { id: u16, seq_number: u16 },
    InformationReply { id: u16, seq_number: u16 },
    Other { data: Vec<u8> },
}

fn parse_icmp_header(input: &[u8]) -> IResult<&[u8], ICMPHeader> {
    let (input, (icmp_type, code, checksum)) = permutation((be_u8, be_u8, be_u16))(input)?;
    Ok((input, ICMPHeader { icmp_type, code, checksum }))
}

fn parse_icmp_message<'a>(input: &'a [u8], header: &ICMPHeader) -> IResult<&'a [u8], ICMPMessage> {
    match header.icmp_type {
        0 => map_parser(take(8usize), permutation((be_u16, be_u16)))(input)
            .map(|(next_input, (id, seq_number))| (next_input, ICMPMessage::EchoReply { id, seq_number })),
        8 => map_parser(take(8usize), permutation((be_u16, be_u16)))(input)
            .map(|(next_input, (id, seq_number))| (next_input, ICMPMessage::EchoRequest { id, seq_number })),
        13 | 14 => map_parser(take(16usize), permutation((be_u16, be_u16, be_u32, be_u32, be_u32)))(input)
            .map(|(next_input, (id, seq_number, originate_timestamp, receive_timestamp, transmit_timestamp))| {
                (next_input, ICMPMessage::Timestamp { id, seq_number, originate_timestamp, receive_timestamp, transmit_timestamp })
            }),
        _ => Ok((input, ICMPMessage::Other { data: input.to_vec() })),
    }
}

fn read_icmp_packet(input: &[u8]) -> IResult<&[u8], ICMPMessage> {
    let (input, header) = parse_icmp_header(input)?;
    parse_icmp_message(input, &header)
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <file>", args[0]);
        return Ok(())
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match read_icmp_packet(&buffer) {
        Ok((_, message)) => println!("{:?}", message),
        Err(e) => println!("Failed to parse ICMP: {:?}", e),
    }
    
    Ok(())
}