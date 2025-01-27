use std::env;
use std::fs::File;
use std::io::Read;
use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt, value},
    number::complete::{be_u16, be_u32, be_u64},
    sequence::{preceded, tuple},
    IResult,
};

#[derive(Debug)]
struct NitfHeader {
    file_header: FileHeader,
    // Add other header structures as needed based on NITF specification
}


#[derive(Debug)]
struct FileHeader {
    id: String,
    version: String,
    // Add other fields as needed based on NITF specification
}


fn file_header(input: &[u8]) -> IResult<&[u8], FileHeader> {
    let (input, id) = map_res(take(2usize), std::str::from_utf8)(input)?;
    let (input, version) = map_res(take(2usize), std::str::from_utf8)(input)?;

    Ok((input,FileHeader {id: id.to_string(), version:version.to_string()}))
}


fn nitf_header(input: &[u8]) -> IResult<&[u8], NitfHeader> {
    let (input, file_header) = file_header(input)?;
   Ok((input, NitfHeader { file_header }))
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <NITF_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = match File::open(filename) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Error opening file {}: {}", filename, err);
            return;
        }
    };

    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Ok(_) => (),
        Err(err) => {
            eprintln!("Error reading file: {}", err);
            return;
        }
    };

    match nitf_header(&buffer) {
        Ok((_, header)) => println!("NITF Header: {:?}", header),
        Err(err) => {
            eprintln!("Error parsing NITF header: {:?}", err);

        }
    }
}

