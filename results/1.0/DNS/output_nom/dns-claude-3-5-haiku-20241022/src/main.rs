use nom::{
    bits::{streaming::take as take_bits, streaming::tag as tag_bits},
    combinator::{map, opt, complete},
    multi::{count, many0},
    number::streaming::{be_u8, be_u16, be_u32},
    sequence::{tuple, preceded},
    IResult, Err as NomErr,
};
use std::env;
use std::fs::File;
use std::io::Read;

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
    qname: Vec<String>,
    qtype: u16,
    qclass: u16,
}

#[derive(Debug)]
struct DNSResourceRecord {
    name: Vec<String>,
    rtype: u16,
    rclass: u16,
    ttl: u32,
    rdlength: u16,
    rdata: Vec<u8>,
}

#[derive(Debug)]
struct DNSPacket {
    header: DNSHeader,
    questions: Vec<DNSQuestion>,
    answers: Vec<DNSResourceRecord>,
    authorities: Vec<DNSResourceRecord>,
    additionals: Vec<DNSResourceRecord>,
}

fn parse_name(input: &[u8]) -> IResult<&[u8], Vec<String>> {
    let mut labels = Vec::new();
    let mut remaining = input;

    loop {
        let (rest, length) = be_u8(remaining)?;
        if length == 0 {
            return Ok((rest, labels));
        }

        if length & 0xC0 == 0xC0 {
            let (rest, offset) = map(
                tuple((tag_bits(2, 0b11), be_u8)),
                |(_, offset)| offset
            )(remaining)?;
            // TODO: Implement pointer resolution
            return Ok((rest, labels));
        }

        let (rest, label) = map(
            count(be_u8, length as usize),
            |bytes| String::from_utf8_lossy(&bytes).into_owned()
        )(rest)?;

        labels.push(label);
        remaining = rest;
    }
}

fn parse_header(input: &[u8]) -> IResult<&[u8], DNSHeader> {
    map(
        tuple((
            be_u16,
            map(be_u8, |byte| DNSHeader {
                id: 0,
                qr: (byte & 0x80) != 0,
                opcode: (byte & 0x78) >> 3,
                aa: (byte & 0x04) != 0,
                tc: (byte & 0x02) != 0,
                rd: (byte & 0x01) != 0,
                ra: false,
                z: 0,
                rcode: 0,
                qdcount: 0,
                ancount: 0,
                nscount: 0,
                arcount: 0,
            }),
            map(be_u8, |byte| DNSHeader {
                ra: (byte & 0x80) != 0,
                z: (byte & 0x70) >> 4,
                rcode: byte & 0x0F,
                ..Default::default()
            }),
            be_u16, be_u16, be_u16, be_u16
        )),
        |(id, mut header1, header2, qdcount, ancount, nscount, arcount)| {
            header1.id = id;
            header1.ra = header2.ra;
            header1.z = header2.z;
            header1.rcode = header2.rcode;
            header1.qdcount = qdcount;
            header1.ancount = ancount;
            header1.nscount = nscount;
            header1.arcount = arcount;
            header1
        }
    )(input)
}

fn parse_question(input: &[u8]) -> IResult<&[u8], DNSQuestion> {
    map(
        tuple((
            parse_name,
            be_u16,
            be_u16
        )),
        |(qname, qtype, qclass)| DNSQuestion {
            qname,
            qtype,
            qclass,
        }
    )(input)
}

fn parse_resource_record(input: &[u8]) -> IResult<&[u8], DNSResourceRecord> {
    map(
        tuple((
            parse_name,
            be_u16,
            be_u16,
            be_u32,
            be_u16,
            map(count(be_u8, |length| length as usize), |rdata| rdata)
        )),
        |(name, rtype, rclass, ttl, rdlength, rdata)| DNSResourceRecord {
            name,
            rtype,
            rclass,
            ttl,
            rdlength,
            rdata,
        }
    )(input)
}

fn parse_dns_packet(input: &[u8]) -> IResult<&[u8], DNSPacket> {
    map(
        tuple((
            parse_header,
            count(parse_question, |count| count as usize),
            count(parse_resource_record, |count| count as usize),
            count(parse_resource_record, |count| count as usize),
            count(parse_resource_record, |count| count as usize)
        )),
        |(header, questions, answers, authorities, additionals)| DNSPacket {
            header,
            questions,
            answers,
            authorities,
            additionals,
        }
    )(input)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <dns_packet_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_dns_packet(&buffer) {
        Ok((_, packet)) => {
            println!("Parsed DNS Packet: {:?}", packet);
            Ok(())
        },
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            Err(Box::new(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "Failed to parse DNS packet"
            )))
        }
    }
}

impl Default for DNSHeader {
    fn default() -> Self {
        DNSHeader {
            id: 0,
            qr: false,
            opcode: 0,
            aa: false,
            tc: false,
            rd: false,
            ra: false,
            z: 0,
            rcode: 0,
            qdcount: 0,
            ancount: 0,
            nscount: 0,
            arcount: 0,
        }
    }
}