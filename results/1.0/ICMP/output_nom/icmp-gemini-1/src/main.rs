use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::env;
use std::fs::read;

#[derive(Debug)]
struct IcmpHeader {
    type_: u8,
    code: u8,
    checksum: u16,
    rest: Vec<u8>,
}

fn icmp_header(input: &[u8]) -> IResult<&[u8], IcmpHeader> {
    let (rest, type_) = be_u8(input)?;
    let (rest, code) = be_u8(rest)?;
    let (rest, checksum) = be_u16(rest)?;
    let (rest, rest_data) = take(input.len() - 3)(rest)?;

    Ok((
        &[],
        IcmpHeader {
            type_: type_,
            code: code,
            checksum: checksum,
            rest: rest_data.to_vec(),
        },
    ))
}


fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <icmp_file>", args[0]);
        return Ok(());
    }

    let filename = &args[1];
    let buffer = read(filename)?;

    match icmp_header(&buffer) {
        Ok((_, icmp_header)) => {
            println!("ICMP Header: {:?}", icmp_header);
        }
        Err(e) => {
            eprintln!("Error parsing ICMP header: {:?}", e);
        }
    }

    Ok(())
}

