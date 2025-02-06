use nom::{
    bytes::complete::{take},
    combinator::{map_res},
    number::complete::{be_u16, be_u32, u8},
    sequence::tuple,
    IResult,
};
use std::{env, fs, io::Read};

#[derive(Debug)]
struct DNSHeader {
    id: u16,
    qr: bool,
    opcode: u8,
    aa: bool,
    tc: bool,
    rd: bool,
    ra: bool,
    z: u8,
    rcode: u8,
    qdcount: u16,
    ancount: u16,
    nscount: u16,
    arcount: u16,
}

#[derive(Debug)]
struct DNSQuestion {
    qname: String,
    qtype: u16,
    qclass: u16,
}

#[derive(Debug)]
struct DNSResourceRecord {
    name: String,
    type_: u16,
    class: u16,
    ttl: u32,
    rdlength: u16,
    rdata: Vec<u8>,
}

#[derive(Debug)]
struct DNSMessage {
    header: DNSHeader,
    questions: Vec<DNSQuestion>,
    answers: Vec<DNSResourceRecord>,
    authorities: Vec<DNSResourceRecord>,
    additionals: Vec<DNSResourceRecord>,
}

fn parse_dns_header(input: &[u8]) -> IResult<&[u8], DNSHeader> {
    let (input, (id, flags, qdcount, ancount, nscount, arcount)) =
        tuple((be_u16, be_u16, be_u16, be_u16, be_u16, be_u16))(input)?;

    let qr = (flags >> 15) & 0x1 == 1;
    let opcode = ((flags >> 11) & 0xF) as u8;
    let aa = (flags >> 10) & 0x1 == 1;
    let tc = (flags >> 9) & 0x1 == 1;
    let rd = (flags >> 8) & 0x1 == 1;
    let ra = (flags >> 7) & 0x1 == 1;
    let z = ((flags >> 4) & 0x7) as u8;
    let rcode = (flags & 0xF) as u8;

    Ok((
        input,
        DNSHeader {
            id,
            qr,
            opcode,
            aa,
            tc,
            rd,
            ra,
            z,
            rcode,
            qdcount,
            ancount,
            nscount,
            arcount,
        },
    ))
}

fn parse_dns_name(input: &[u8]) -> IResult<&[u8], String> {
    let mut labels = Vec::new();
    let mut input = input;
    loop {
        let (remaining, len) = u8(input)?;
        if len == 0 {
            break;
        }
        let (remaining, label) = take(len)(remaining)?;
        labels.push(String::from_utf8_lossy(label).to_string());
        input = remaining;
    }
    Ok((input, labels.join(".")))
}

fn parse_dns_question(input: &[u8]) -> IResult<&[u8], DNSQuestion> {
    let (input, qname) = parse_dns_name(input)?;
    let (input, qtype) = be_u16(input)?;
    let (input, qclass) = be_u16(input)?;
    Ok((
        input,
        DNSQuestion {
            qname,
            qtype,
            qclass,
        },
    ))
}

fn parse_dns_resource_record(input: &[u8]) -> IResult<&[u8], DNSResourceRecord> {
    let (input, name) = parse_dns_name(input)?;
    let (input, type_) = be_u16(input)?;
    let (input, class) = be_u16(input)?;
    let (input, ttl) = be_u32(input)?;
    let (input, rdlength) = be_u16(input)?;
    let (input, rdata) = take(rdlength)(input)?;
    Ok((
        input,
        DNSResourceRecord {
            name,
            type_,
            class,
            ttl,
            rdlength,
            rdata: rdata.to_vec(),
        },
    ))
}

fn parse_dns_message(input: &[u8]) -> IResult<&[u8], DNSMessage> {
    let (input, header) = parse_dns_header(input)?;
    let (input, questions) =
        map_res(take(header.qdcount as usize), |data: &[u8]| {
            let mut questions = Vec::new();
            let mut input = data;
            for _ in 0..header.qdcount {
                let (remaining, question) = parse_dns_question(input)?;
                questions.push(question);
                input = remaining;
            }
            Ok::<_, nom::Err<nom::error::Error<&[u8]>>>(questions)
        })(input)?;
    let (input, answers) =
        map_res(take(header.ancount as usize), |data: &[u8]| {
            let mut answers = Vec::new();
            let mut input = data;
            for _ in 0..header.ancount {
                let (remaining, answer) = parse_dns_resource_record(input)?;
                answers.push(answer);
                input = remaining;
            }
            Ok::<_, nom::Err<nom::error::Error<&[u8]>>>(answers)
        })(input)?;
    let (input, authorities) =
        map_res(take(header.nscount as usize), |data: &[u8]| {
            let mut authorities = Vec::new();
            let mut input = data;
            for _ in 0..header.nscount {
                let (remaining, authority) = parse_dns_resource_record(input)?;
                authorities.push(authority);
                input = remaining;
            }
            Ok::<_, nom::Err<nom::error::Error<&[u8]>>>(authorities)
        })(input)?;
    let (input, additionals) =
        map_res(take(header.arcount as usize), |data: &[u8]| {
            let mut additionals = Vec::new();
            let mut input = data;
            for _ in 0..header.arcount {
                let (remaining, additional) = parse_dns_resource_record(input)?;
                additionals.push(additional);
                input = remaining;
            }
            Ok::<_, nom::Err<nom::error::Error<&[u8]>>>(additionals)
        })(input)?;
    Ok((
        input,
        DNSMessage {
            header,
            questions,
            answers,
            authorities,
            additionals,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_dns_file>", args[0]);
        return;
    }

    let mut file = fs::File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_dns_message(&buffer) {
        Ok((_, dns_message)) => println!("{:#?}", dns_message),
        Err(e) => eprintln!("Parsing failed: {:?}", e),
    }
}