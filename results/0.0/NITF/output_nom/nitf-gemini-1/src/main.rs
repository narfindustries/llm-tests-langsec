use nom::{
    bytes::complete::{tag, take_while, take_while1},
    combinator::{map, map_res, opt, recognize},
    number::complete::{be_u16, be_u32, be_u64},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug)]
struct NitfHeader {
    file_header: FileHeader,
    // Add other fields as needed based on NITF specification
}


#[derive(Debug)]
struct FileHeader {
    header_length: u32,
    file_name: String,
    // Add other File Header fields here
}

fn parse_file_header(input: &[u8]) -> IResult<&[u8], FileHeader> {
    let (input, header_length) = be_u32(input)?;
    let (input, file_name) = take_while1(|c| c != 0)(input)?;
    let file_name_str = std::str::from_utf8(file_name).unwrap().to_string();

    Ok((input, FileHeader { header_length, file_name: file_name_str }))

}


fn parse_nitf_header(input: &[u8]) -> IResult<&[u8], NitfHeader> {
    let (input, file_header) = parse_file_header(input)?;
    // Parse other header fields here based on NITF specification

    Ok((input, NitfHeader { file_header }))
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <NITF_file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let mut file = match File::open(filename) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Error opening file {}: {}", filename, err);
            std::process::exit(1);
        }
    };

    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Ok(_) => (),
        Err(err) => {
            eprintln!("Error reading file {}: {}", filename, err);
            std::process::exit(1);
        }
    };

    match parse_nitf_header(&buffer) {
        Ok((_, header)) => println!("NITF Header: {:?}", header),
        Err(err) => {
            eprintln!("Error parsing NITF header: {:?}", err);
            std::process::exit(1);
        }
    }
}
