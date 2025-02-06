use nom::{
    bytes::complete::take,
    error::ErrorKind,
    number::complete::{be_u16, be_u32},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

// DNS Header Structure
#[derive(Debug)]
struct DnsHeader {
    id: u16,
    flags: u16,
    qdcount: u16,
    ancount: u16,
    nscount: u16,
    arcount: u16,
}

// DNS Question Section
#[derive(Debug)]
struct DnsQuestion {
    qname: String,
    qtype: u16,
    qclass: u16,
}

// DNS Resource Record
#[derive(Debug)]
struct DnsResourceRecord {
    name: String,
    rrtype: u16,
    class: u16,
    ttl: u32,
    rdlength: u16,
    rdata: Vec<u8>,
}

fn parse_qname(input: &[u8]) -> IResult<&[u8], String> {
    let mut offset = 0;
    let mut labels = Vec::new();

    while offset < input.len() {
        let len = input[offset] as usize;
        if len == 0 {
            offset += 1;
            break;
        }
        offset += 1;
        labels.push(&input[offset..offset + len]);
        offset += len;
    }

    let qname = labels
        .iter()
        .map(|label| {
            std::str::from_utf8(label)
                .map(|str| str.to_string())
                .map_err(|_| nom::Err::Error((input, ErrorKind::Char)))
        })
        .collect::<Result<Vec<_>, _>>()?
        .join(".");
    Ok((&input[offset..], qname))
}

fn parse_dns_header(input: &[u8]) -> IResult<&[u8], DnsHeader> {
    let (input, (id, flags, qdcount, ancount, nscount, arcount)) =
        tuple((be_u16, be_u16, be_u16, be_u16, be_u16, be_u16))(input)?;
    Ok((
        input,
        DnsHeader {
            id,
            flags,
            qdcount,
            ancount,
            nscount,
            arcount,
        },
    ))
}

fn parse_dns_question(input: &[u8]) -> IResult<&[u8], DnsQuestion> {
    let (input, qname) = parse_qname(input)?;
    let (input, (qtype, qclass)) = tuple((be_u16, be_u16))(input)?;
    Ok((
        input,
        DnsQuestion {
            qname,
            qtype,
            qclass,
        },
    ))
}

fn parse_dns_rr(input: &[u8]) -> IResult<&[u8], DnsResourceRecord> {
    let (input, name) = parse_qname(input)?;
    let (input, (rrtype, class, ttl, rdlength)) = tuple((be_u16, be_u16, be_u32, be_u16))(input)?;
    let (input, rdata) = take(rdlength as usize)(input)?;
    Ok((
        input,
        DnsResourceRecord {
            name,
            rrtype,
            class,
            ttl,
            rdlength,
            rdata: rdata.to_vec(),
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("Usage: {} <dns_message_file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_dns_header(&buffer) {
        Ok((input, header)) => {
            println!("{:?}", header);

            let mut input = input;
            for _ in 0..header.qdcount {
                match parse_dns_question(input) {
                    Ok((new_input, question)) => {
                        println!("{:?}", question);
                        input = new_input;
                    }
                    Err(e) => {
                        eprintln!("Failed to parse question: {:?}", e);
                        return;
                    }
                }
            }

            for _ in 0..header.ancount {
                match parse_dns_rr(input) {
                    Ok((new_input, answer)) => {
                        println!("{:?}", answer);
                        input = new_input;
                    }
                    Err(e) => {
                        eprintln!("Failed to parse answer: {:?}", e);
                        return;
                    }
                }
            }

            for _ in 0..header.nscount {
                match parse_dns_rr(input) {
                    Ok((new_input, authority)) => {
                        println!("{:?}", authority);
                        input = new_input;
                    }
                    Err(e) => {
                        eprintln!("Failed to parse authority: {:?}", e);
                        return;
                    }
                }
            }

            for _ in 0..header.arcount {
                match parse_dns_rr(input) {
                    Ok((new_input, additional)) => {
                        println!("{:?}", additional);
                        input = new_input;
                    }
                    Err(e) => {
                        eprintln!("Failed to parse additional: {:?}", e);
                        return;
                    }
                }
            }
        }
        Err(e) => {
            eprintln!("Failed to parse DNS header: {:?}", e);
        }
    }
}