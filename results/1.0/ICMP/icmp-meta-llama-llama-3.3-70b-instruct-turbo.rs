use nom::{
    bytes::complete::{take, take_till},
    IResult,
};
use std::{
    env, fs::File, io::Read, path::Path,
};

#[derive(Debug, Clone, Copy)]
enum IcmpType {
    EchoReply,
    DestinationUnreachable,
    SourceQuench,
    Redirect,
    EchoRequest,
    RouterAdvertisement,
    RouterSolicitation,
    TimeExceeded,
    ParameterProblem,
    TimestampRequest,
    TimestampReply,
    InformationRequest,
    InformationReply,
    Unassigned(u8),
}

#[derive(Debug, Clone, Copy)]
enum IcmpCode {
    NetUnreachable,
    HostUnreachable,
    ProtocolUnreachable,
    PortUnreachable,
    FragmentationNeededAndDontFragmentWasSet,
    SourceRouteFailed,
    RedirectForNetwork,
    RedirectForHost,
    RedirectForTypeOfServiceAndNetwork,
    RedirectForTypeOfServiceAndHost,
    TimeToLiveExceededInTransit,
    FragmentReassemblyTimeExceeded,
    PointerIndicatesTheError,
    MissingARequiredOption,
    Unassigned(u8),
}

#[derive(Debug)]
struct IcmpHeader {
    ty: IcmpType,
    code: IcmpCode,
    checksum: u16,
    identifier: u16,
    sequence_number: u16,
}

#[derive(Debug)]
enum IcmpMessage {
    EchoRequest {
        header: IcmpHeader,
        data: Vec<u8>,
    },
    EchoReply {
        header: IcmpHeader,
        data: Vec<u8>,
    },
    DestinationUnreachable {
        header: IcmpHeader,
        unused: u32,
        internet_header: Vec<u8>,
    },
    TimeExceeded {
        header: IcmpHeader,
        unused: u32,
        internet_header: Vec<u8>,
    },
}

fn parse_icmp_type(input: &[u8]) -> IResult<&[u8], IcmpType> {
    let (input, ty) = take(1usize)(input)?;
    match ty[0] {
        0 => Ok((input, IcmpType::EchoReply)),
        3 => Ok((input, IcmpType::DestinationUnreachable)),
        4 => Ok((input, IcmpType::SourceQuench)),
        5 => Ok((input, IcmpType::Redirect)),
        8 => Ok((input, IcmpType::EchoRequest)),
        9 => Ok((input, IcmpType::RouterAdvertisement)),
        10 => Ok((input, IcmpType::RouterSolicitation)),
        11 => Ok((input, IcmpType::TimeExceeded)),
        12 => Ok((input, IcmpType::ParameterProblem)),
        13 => Ok((input, IcmpType::TimestampRequest)),
        14 => Ok((input, IcmpType::TimestampReply)),
        15 => Ok((input, IcmpType::InformationRequest)),
        16 => Ok((input, IcmpType::InformationReply)),
        _ => Ok((input, IcmpType::Unassigned(ty[0]))),
    }
}

fn parse_icmp_code(input: &[u8], ty: IcmpType) -> IResult<&[u8], IcmpCode> {
    let (input, code) = take(1usize)(input)?;
    match ty {
        IcmpType::DestinationUnreachable => match code[0] {
            0 => Ok((input, IcmpCode::NetUnreachable)),
            1 => Ok((input, IcmpCode::HostUnreachable)),
            2 => Ok((input, IcmpCode::ProtocolUnreachable)),
            3 => Ok((input, IcmpCode::PortUnreachable)),
            4 => Ok((input, IcmpCode::FragmentationNeededAndDontFragmentWasSet)),
            5 => Ok((input, IcmpCode::SourceRouteFailed)),
            _ => Ok((input, IcmpCode::Unassigned(code[0]))),
        },
        IcmpType::Redirect => match code[0] {
            0 => Ok((input, IcmpCode::RedirectForNetwork)),
            1 => Ok((input, IcmpCode::RedirectForHost)),
            2 => Ok((input, IcmpCode::RedirectForTypeOfServiceAndNetwork)),
            3 => Ok((input, IcmpCode::RedirectForTypeOfServiceAndHost)),
            _ => Ok((input, IcmpCode::Unassigned(code[0]))),
        },
        IcmpType::TimeExceeded => match code[0] {
            0 => Ok((input, IcmpCode::TimeToLiveExceededInTransit)),
            1 => Ok((input, IcmpCode::FragmentReassemblyTimeExceeded)),
            _ => Ok((input, IcmpCode::Unassigned(code[0]))),
        },
        IcmpType::ParameterProblem => match code[0] {
            0 => Ok((input, IcmpCode::PointerIndicatesTheError)),
            1 => Ok((input, IcmpCode::MissingARequiredOption)),
            _ => Ok((input, IcmpCode::Unassigned(code[0]))),
        },
        _ => Ok((input, IcmpCode::Unassigned(code[0]))),
    }
}

fn parse_icmp_header(input: &[u8]) -> IResult<&[u8], IcmpHeader> {
    let (input, ty) = parse_icmp_type(input)?;
    let (input, code) = parse_icmp_code(input, ty)?;
    let (input, checksum) = take(2usize)(input)?;
    let checksum = u16::from_be_bytes([checksum[0], checksum[1]]);
    let (input, identifier) = take(2usize)(input)?;
    let identifier = u16::from_be_bytes([identifier[0], identifier[1]]);
    let (input, sequence_number) = take(2usize)(input)?;
    let sequence_number = u16::from_be_bytes([sequence_number[0], sequence_number[1]]);
    Ok((input, IcmpHeader {
        ty,
        code,
        checksum,
        identifier,
        sequence_number,
    }))
}

fn parse_icmp_message(input: &[u8]) -> IResult<&[u8], IcmpMessage> {
    let (input, header) = parse_icmp_header(input)?;
    match header.ty {
        IcmpType::EchoRequest | IcmpType::EchoReply => {
            let (input, data) = take_till(|_| false)(input)?;
            Ok((input, IcmpMessage::EchoRequest { header, data: data.to_vec() }))
        }
        IcmpType::DestinationUnreachable => {
            let (input, unused) = take(4usize)(input)?;
            let unused = u32::from_be_bytes([unused[0], unused[1], unused[2], unused[3]]);
            let (input, internet_header) = take_till(|_| false)(input)?;
            Ok((input, IcmpMessage::DestinationUnreachable {
                header,
                unused,
                internet_header: internet_header.to_vec(),
            }))
        }
        IcmpType::TimeExceeded => {
            let (input, unused) = take(4usize)(input)?;
            let unused = u32::from_be_bytes([unused[0], unused[1], unused[2], unused[3]]);
            let (input, internet_header) = take_till(|_| false)(input)?;
            Ok((input, IcmpMessage::TimeExceeded {
                header,
                unused,
                internet_header: internet_header.to_vec(),
            }))
        }
        _ => Ok((input, IcmpMessage::EchoRequest { header, data: vec![] })),
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <filename>", args[0]);
        return;
    }
    let filename = &args[1];
    let path = Path::new(filename);
    let display = path.display();
    let mut file = match File::open(&path) {
        Err(why) => panic!("Couldn't open {}: {}", display, why),
        Ok(file) => file,
    };
    let mut data = vec![];
    file.read_to_end(&mut data).unwrap();
    match parse_icmp_message(&data) {
        Ok((_, msg)) => println!("{:?}", msg),
        Err(err) => println!("{:?}", err),
    }
}