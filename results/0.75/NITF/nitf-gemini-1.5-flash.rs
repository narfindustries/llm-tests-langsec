use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt},
    number::complete::{be_u16, be_u32, be_u64},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct FileHeader {
    version: String,
    security: String,
    length: u64,
    // ... other File Header fields ...
}

#[derive(Debug)]
struct ImageHeader {
    number_of_images: u16,
    // ... other Image Header fields ...
}


fn file_header(input: &[u8]) -> IResult<&[u8], FileHeader> {
    let (input, version) = map_res(take(2usize), std::str::from_utf8)(input)?;
    let (input, security) = map_res(take(2usize), std::str::from_utf8)(input)?;
    let (input, length) = be_u64(input)?;

    Ok((
        input,
        FileHeader {
            version: version.to_string(),
            security: security.to_string(),
            length,
            // ... other File Header fields
        },
    ))
}

fn image_header(input: &[u8]) -> IResult<&[u8], ImageHeader> {
    let (input, number_of_images) = be_u16(input)?;
    Ok((
        input,
        ImageHeader {
            number_of_images,
            // ... other Image Header fields
        },
    ))
}

fn nitf_parser(input: &[u8]) -> IResult<&[u8], (FileHeader, ImageHeader)> {
    let (input, file_header) = file_header(input)?;
    let (input, image_header) = image_header(input)?;
    Ok((input, (file_header, image_header)))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <NITF_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match nitf_parser(&buffer) {
        Ok((_, (file_header, image_header))) => {
            println!("File Header: {:?}", file_header);
            println!("Image Header: {:?}", image_header);
        }
        Err(e) => {
            eprintln!("Error parsing NITF file: {:?}", e);
        }
    }
}
