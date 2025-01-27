use std::env;
use std::fs::File;
use std::io::Read;
use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt, recognize},
    number::complete::{be_u16, be_u32, be_u64},
    sequence::{preceded, tuple},
    IResult,
};

#[derive(Debug)]
struct NitfHeader {
    file_header: FileHeader,
    // Add other fields as needed based on the NITF specification
}


#[derive(Debug)]
struct FileHeader {
    file_header_length: u32,
    unique_id: String,
    // Add other fields as needed based on the NITF specification

}


fn file_header(input: &[u8]) -> IResult<&[u8], FileHeader> {
    let (input, file_header_length) = be_u32(input)?;
    let (input, unique_id) = take(20usize)(input)?;
    let unique_id_str = std::str::from_utf8(unique_id).unwrap().trim_end_matches('\0');


    Ok((input, FileHeader {
        file_header_length,
        unique_id: unique_id_str.to_string(),
       }))
}


fn nitf_header(input: &[u8]) -> IResult<&[u8], NitfHeader> {
    let (input, file_header) = file_header(input)?;

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

    match nitf_header(&buffer) {
        Ok((remaining, header)) => {
            println!("NITF Header: {:?}", header);
            println!("Remaining bytes: {} bytes", remaining.len());

        },
        Err(err) => {
            eprintln!("Error parsing NITF header: {:?}", err);
            std::process::exit(1);
        }
    }
}
