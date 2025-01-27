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
struct DNSHeader {
    id: u16,
    qr: u8,
    opcode: u8,
    aa: u8,
    tc: u8,
    rd: u8,
    ra: u8,
    z: u8,
    rcode: u8,
    qdcount: u16,
    ancount: u16,
    nscount: u16,
    arcount: u16,
}

#[derive(Debug)]
struct Question {
    qname: Vec<u8>,
    qtype: u16,
    qclass: u16,
}

#[derive(Debug)]
struct ResourceRecord {
    name: Vec<u8>,
    rtype: u16,
    rclass: u16,
    ttl: u32,
    rdlength: u16,
    rdata: Vec<u8>,
}

#[derive(Debug)]
struct DNSPacket {
    header: DNSHeader,
    questions: Vec<Question>,
    answers: Vec<ResourceRecord>,
    authorities: Vec<ResourceRecord>,
    additionals: Vec<ResourceRecord>,
}

fn parse_u16(input: &[u8]) -> IResult<&[u8], u16> {
    be_u16(input)
}

fn parse_u32(input: &[u8]) -> IResult<&[u8], u32> {
    be_u32(input)
}

fn parse_dns_header(input: &[u8]) -> IResult<&[u8], DNSHeader> {
    let (input, id) = parse_u16(input)?;
    let (input, flags) = parse_u16(input)?;
    let qr = (flags >> 15) & 0x1;
    let opcode = (flags >> 11) & 0xF;
    let aa = (flags >> 10) & 0x1;
    let tc = (flags >> 9) & 0x1;
    let rd = (flags >> 8) & 0x1;
    let ra = (flags >> 7) & 0x1;
    let z = (flags >> 4) & 0x7;
    let rcode = flags & 0xF;
    let (input, qdcount) = parse_u16(input)?;
    let (input, ancount) = parse_u16(input)?;
    let (input, nscount) = parse_u16(input)?;
    let (input, arcount) = parse_u16(input)?;

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

fn parse_dns_question(input: &[u8], count: u16) -> IResult<&[u8], Vec<Question>> {
    let mut input = input;
    let mut questions = Vec::with_capacity(count as usize);

    for _ in 0..count {
        let (i, qname) = take_while(|b: u8| b != 0)(input)?;
        let (i, _) = take(1usize)(i)?; // skip the null byte
        let (i, qtype) = parse_u16(i)?;
        let (i, qclass) = parse_u16(i)?;

        questions.push(Question {
            qname: qname.to_vec(),
            qtype,
            qclass,
        });

        input = i;
    }

    Ok((input, questions))
}

fn parse_dns_rr(input: &[u8], count: u16) -> IResult<&[u8], Vec<ResourceRecord>> {
    let mut input = input;
    let mut records = Vec::with_capacity(count as usize);

    for _ in 0..count {
        let (i, name) = take_while(|b: u8| b != 0)(input)?;
        let (i, _) = take(1usize)(i)?; // skip the null byte
        let (i, rtype) = parse_u16(i)?;
        let (i, rclass) = parse_u16(i)?;
        let (i, ttl) = parse_u32(i)?;
        let (i, rdlength) = parse_u16(i)?;
        let (i, rdata) = take(rdlength)(i)?;

        records.push(ResourceRecord {
            name: name.to_vec(),
            rtype,
            rclass,
            ttl,
            rdlength,
            rdata: rdata.to_vec(),
        });

        input = i;
    }

    Ok((input, records))
}

fn parse_dns_packet(input: &[u8]) -> IResult<&[u8], DNSPacket> {
    let (input, header) = parse_dns_header(input)?;
    let (input, questions) = parse_dns_question(input, header.qdcount)?;
    let (input, answers) = parse_dns_rr(input, header.ancount)?;
    let (input, authorities) = parse_dns_rr(input, header.nscount)?;
    let (input, additionals) = parse_dns_rr(input, header.arcount)?;

    Ok((
        input,
        DNSPacket {
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
    if args.len() < 2 {
        eprintln!("Usage: {} <DNS_packet_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Unable to open file");
    let mut data = Vec::new();
    file.read_to_end(&mut data).expect("Unable to read data");

    match parse_dns_packet(&data) {
        Ok((_, packet)) => {
            println!("{:#?}", packet);
        }
        Err(e) => {
            eprintln!("Failed to parse DNS packet: {:?}", e);
        }
    }
}