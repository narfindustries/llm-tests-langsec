use nom::{
    be_u8, be_u16, be_u32,
    bytes::complete::take,
    combinator::map,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct IcmpHeader {
    type_: u8,
    code: u8,
    checksum: u16,
    identifier: u16,
    sequence_number: u16,
    data: Vec<u8>,
}


fn icmp_header(input: &[u8]) -> IResult<&[u8], IcmpHeader> {
    let (rest, type_) = be_u8(input)?;
    let (rest, code) = be_u8(rest)?;
    let (rest, checksum) = be_u16(rest)?;
    let (rest, identifier) = be_u16(rest)?;
    let (rest, sequence_number) = be_u16(rest)?;

    // Handle optional data
    let data_len = match input.len() {
        len if len > 8 => len - 8,
        _ => 0,

    };
    let (rest, data) = take(data_len)(rest)?;

    Ok((
        rest,
        IcmpHeader {
            type_,
            code,
            checksum,
            identifier,
            sequence_number,
            data: data.to_vec(),
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = match File::open(filename) {
        Ok(file) => file,
        Err(err) => {
            println!("Error opening file: {}", err);
            return;
        }
    };

    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Ok(_) => (),
        Err(err) => {
            println!("Error reading file: {}", err);
            return;
        }
    };


    match icmp_header(&buffer) {
        Ok((_, header)) => {
            println!("ICMP Header: {:?}", header);
        }
        Err(err) => {
            println!("Error parsing ICMP header: {:?}", err);
        }
    }
}

