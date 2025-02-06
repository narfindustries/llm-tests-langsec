use nom::{
    number::complete::{be_u16, be_u32},
    IResult,
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
struct DNSQuestion {
    qname: Vec<u8>,
    qtype: u16,
    qclass: u16,
}

#[derive(Debug)]
struct DNSRecord {
    name: Vec<u8>,
    rtype: u16,
    class: u16,
    ttl: u32,
    rdlength: u16,
    rdata: Vec<u8>,
}

#[derive(Debug)]
struct DNSPacket {
    header: DNSHeader,
    questions: Vec<DNSQuestion>,
    answers: Vec<DNSRecord>,
    authorities: Vec<DNSRecord>,
    additionals: Vec<DNSRecord>,
}

fn parse_dns_header(input: &[u8]) -> IResult<&[u8], DNSHeader> {
    let (input, id) = be_u16(input)?;
    let (input, flags) = be_u16(input)?;
    let (input, qdcount) = be_u16(input)?;
    let (input, ancount) = be_u16(input)?;
    let (input, nscount) = be_u16(input)?;
    let (input, arcount) = be_u16(input)?;

    let qr = ((flags >> 15) & 0x01) as u8;
    let opcode = ((flags >> 11) & 0x0F) as u8;
    let aa = ((flags >> 10) & 0x01) as u8;
    let tc = ((flags >> 9) & 0x01) as u8;
    let rd = ((flags >> 8) & 0x01) as u8;
    let ra = ((flags >> 7) & 0x01) as u8;
    let z = ((flags >> 4) & 0x07) as u8;
    let rcode = (flags & 0x0F) as u8;

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

fn parse_dns_question(input: &[u8], count: u16) -> IResult<&[u8], Vec<DNSQuestion>> {
    let mut input = input;
    let mut questions = Vec::new();

    for _ in 0..count {
        let (next_input, qname) = parse_qname(input)?;
        let (next_input, qtype) = be_u16(next_input)?;
        let (next_input, qclass) = be_u16(next_input)?;
        questions.push(DNSQuestion {
            qname,
            qtype,
            qclass,
        });
        input = next_input;
    }

    Ok((input, questions))
}

fn parse_qname(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let mut input = input;
    let mut qname = Vec::new();
    loop {
        let (next_input, length) = nom::number::complete::be_u8(input)?;
        if length == 0 {
            break;
        }
        let (next_input, label) = nom::bytes::complete::take(length)(next_input)?;
        qname.extend_from_slice(label);
        input = next_input;
    }
    Ok((input, qname))
}

fn parse_dns_record(input: &[u8], count: u16) -> IResult<&[u8], Vec<DNSRecord>> {
    let mut input = input;
    let mut records = Vec::new();

    for _ in 0..count {
        let (next_input, name) = parse_qname(input)?;
        let (next_input, rtype) = be_u16(next_input)?;
        let (next_input, class) = be_u16(next_input)?;
        let (next_input, ttl) = be_u32(next_input)?;
        let (next_input, rdlength) = be_u16(next_input)?;
        let (next_input, rdata) = nom::bytes::complete::take(rdlength)(next_input)?;
        records.push(DNSRecord {
            name,
            rtype,
            class,
            ttl,
            rdlength,
            rdata: rdata.to_vec(),
        });
        input = next_input;
    }

    Ok((input, records))
}

fn parse_dns_packet(input: &[u8]) -> IResult<&[u8], DNSPacket> {
    let (input, header) = parse_dns_header(input)?;
    let (input, questions) = parse_dns_question(input, header.qdcount)?;
    let (input, answers) = parse_dns_record(input, header.ancount)?;
    let (input, authorities) = parse_dns_record(input, header.nscount)?;
    let (input, additionals) = parse_dns_record(input, header.arcount)?;

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
    use std::env;
    use std::fs;

    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let data = fs::read(filename).expect("Failed to read file");

    match parse_dns_packet(&data) {
        Ok((_, packet)) => println!("{:#?}", packet),
        Err(e) => eprintln!("Failed to parse DNS packet: {:?}", e),
    }
}