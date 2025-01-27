use nom::{
    bytes::complete::{tag, take_while, take_while1},
    combinator::{map, map_res, opt, recognize},
    error::ErrorKind,
    number::complete::{be_u16, be_u32, be_u64},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::path::Path;

#[derive(Debug)]
struct NitfHeader {
    file_header: FileHeader,
    // Add other fields as needed based on NITF specification
}

#[derive(Debug)]
struct FileHeader {
    header_length: u32,
    file_name: String,
    // Add other fields as needed based on NITF specification
}


fn from_bytes_to_string(input: &[u8]) -> Result<String, std::string::FromUtf8Error> {
    String::from_utf8(input.to_vec())
}

fn is_alphanumeric(c: u8) -> bool {
    (b'a' <= c && c <= b'z') || (b'A' <= c && c <= b'Z') || (b'0' <= c && c <= b'9')
}

fn parse_file_header(input: &[u8]) -> IResult<&[u8], FileHeader> {
    let (input, header_length) = be_u32(input)?;
    let (input, file_name) =  map_res(
        recognize(take_while1(|c| is_alphanumeric(c) )),
        from_bytes_to_string,
    )(input)?;

    // Add parsers for other FileHeader fields here

    Ok((
        input,
        FileHeader {
            header_length,
            file_name,
            // Add other fields here
        },
    ))
}


fn parse_nitf_header(input: &[u8]) -> IResult<&[u8], NitfHeader> {
    let (input, file_header) = parse_file_header(input)?;
    // Add parsers for other NitfHeader fields here

    Ok((
        input,
        NitfHeader { file_header },
    ))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <NITF_file>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let mut file = File::open(path).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_nitf_header(&buffer) {
        Ok((_, header)) => println!("Parsed NITF header: {:?}", header),
        Err(e) => eprintln!("Failed to parse NITF header: {:?}", e),
    }
}
