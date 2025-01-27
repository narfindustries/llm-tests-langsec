// Cargo.toml dependencies
// [dependencies]
// nom = "7.1"
// clap = { version = "4.0", features = ["derive"] }

use clap::Parser;
use nom::{
    bytes::complete::take,
    combinator::map_res,
    number::complete::{be_u16, be_u32, be_u8},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

/// ICMP packet types
#[derive(Debug)]
enum ICMPType {
    EchoReply,
    DestinationUnreachable,
    SourceQuench,
    Redirect,
    EchoRequest,
    TimeExceeded,
    ParameterProblem,
    Timestamp,
    TimestampReply,
    InfoRequest,
    InfoReply,
    AddressMaskRequest,
    AddressMaskReply,
    Unknown(u8),
}

impl From<u8> for ICMPType {
    fn from(value: u8) -> Self {
        match value {
            0 => ICMPType::EchoReply,
            3 => ICMPType::DestinationUnreachable,
            4 => ICMPType::SourceQuench,
            5 => ICMPType::Redirect,
            8 => ICMPType::EchoRequest,
            11 => ICMPType::TimeExceeded,
            12 => ICMPType::ParameterProblem,
            13 => ICMPType::Timestamp,
            14 => ICMPType::TimestampReply,
            15 => ICMPType::InfoRequest,
            16 => ICMPType::InfoReply,
            17 => ICMPType::AddressMaskRequest,
            18 => ICMPType::AddressMaskReply,
            _ => ICMPType::Unknown(value),
        }
    }
}

/// ICMP packet structure
#[derive(Debug)]
struct ICMPPacket {
    icmp_type: ICMPType,
    code: u8,
    checksum: u16,
    rest_of_header: Vec<u8>,
}

fn parse_icmp(input: &[u8]) -> IResult<&[u8], ICMPPacket> {
    let (input, icmp_type_byte) = be_u8(input)?;
    let icmp_type = ICMPType::from(icmp_type_byte);
    let (input, code) = be_u8(input)?;
    let (input, checksum) = be_u16(input)?;
    let (input, rest_of_header) = take(4usize)(input)?;
    Ok((
        input,
        ICMPPacket {
            icmp_type,
            code,
            checksum,
            rest_of_header: rest_of_header.to_vec(),
        },
    ))
}

#[derive(Parser)]
struct Cli {
    /// The binary file to parse
    #[arg(parse(from_os_str))]
    file: PathBuf,
}

fn main() {
    let args = Cli::parse();

    let mut file = File::open(args.file).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_icmp(&buffer) {
        Ok((_, icmp_packet)) => {
            println!("{:?}", icmp_packet);
        }
        Err(err) => {
            eprintln!("Failed to parse ICMP packet: {:?}", err);
        }
    }
}