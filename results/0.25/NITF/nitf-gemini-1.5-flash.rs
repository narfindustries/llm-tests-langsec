use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    number::complete::{be_u16, be_u32},
    sequence::preceded,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;
use std::str;

#[derive(Debug)]
struct NitfHeader {
    signature: String,
    version: String,
    header_length: u32,
    offset_to_image_data: u32,
    image_data_length: u32,
    security_classification: String,
    num_tres: u16,
    tres: Vec<Tre>,
}

#[derive(Debug)]
struct Tre {
    tag: String,
    data: Vec<u8>,
}

fn parse_string(input: &[u8]) -> IResult<&[u8], String> {
    let s = str::from_utf8(input).map(|s| s.to_string()).map_err(|e| nom::Err::Error(nom::error::Error::new(input, e)))?;
    Ok(("", s))
}

fn parse_tre(input: &[u8]) -> IResult<&[u8], Tre> {
    let (input, tag) = take(4usize)(input)?;
    let tag_str = String::from_utf8_lossy(tag).to_string();
    let (input, data_len) = be_u32(input)?;
    let (input, data) = take(data_len as usize)(input)?;
    Ok((input, Tre { tag: tag_str, data: data.to_vec() }))
}

fn parse_nitf_header(input: &[u8]) -> IResult<&[u8], NitfHeader> {
    let (input, signature) = map(take(8usize), parse_string)(input)?;
    let (input, version) = map(take(8usize), parse_string)(input)?;
    let (input, header_length) = be_u32(input)?;
    let (input, offset_to_image_data) = be_u32(input)?;
    let (input, image_data_length) = be_u32(input)?;
    let (input, security_classification) = map(take(20usize), parse_string)(input)?;
    let (input, num_tres) = be_u16(input)?;
    let (input, tres) = map(
        opt(preceded(tag(b"\n"), parse_tre)),
        |tre_opt| {
            let mut tres = Vec::new();
            if let Some(tre) = tre_opt {
                tres.push(tre);
            }
            tres
        },
    )(input)?;

    Ok((
        input,
        NitfHeader {
            signature,
            version,
            header_length,
            offset_to_image_data,
            image_data_length,
            security_classification,
            num_tres,
            tres,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: nitf_parser <filename>");
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_nitf_header(&buffer) {
        Ok((_, header)) => println!("NITF Header: {:?}", header),
        Err(e) => println!("Error parsing NITF header: {:?}", e),
    }
}
