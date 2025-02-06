use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    number::complete::be_u16,
    IResult,
};
use std::fs;
use std::env;

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
    let (rest, type_) = take(1usize)(input)?;
    let (rest, code) = take(1usize)(rest)?;
    let (rest, checksum) = be_u16(rest)?;
    let (rest, identifier) = be_u16(rest)?;
    let (rest, sequence_number) = be_u16(rest)?;

    //Data field is variable length.  We'll need a way to determine its length.  This is simplified; a real implementation would need to handle different ICMP message types appropriately.
    let data_len = rest.len();
    let (rest, data) = take(data_len)(rest)?;

    Ok((
        rest,
        IcmpHeader {
            type_: type_[0],
            code: code[0],
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
        eprintln!("Usage: {} <icmp_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let contents = fs::read(filename).expect("Something went wrong reading the file");

    match icmp_header(&contents) {
        Ok((rest, header)) => {
            println!("Parsed ICMP header:");
            println!("  Type: {}", header.type_);
            println!("  Code: {}", header.code);
            println!("  Checksum: {}", header.checksum);
            println!("  Identifier: {}", header.identifier);
            println!("  Sequence Number: {}", header.sequence_number);
            println!("  Data Length: {}", header.data.len());
            //Further processing of data field based on type and code would go here.

            println!("Remaining bytes: {:?}", rest);

        }
        Err(e) => {
            eprintln!("Error parsing ICMP header: {:?}", e);
        }
    }
}
