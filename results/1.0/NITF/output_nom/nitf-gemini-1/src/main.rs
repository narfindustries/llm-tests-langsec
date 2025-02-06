use nom::{
    bytes::complete::{tag, take_while1},
    number::complete::{be_u16, be_u32},
    sequence::{tuple, preceded},
    IResult, Err, error::ErrorKind,
};
use std::env;
use std::fs::File;
use std::io::Read;
use std::str;

#[derive(Debug)]
struct NitfFileHeader {
    file_header_length: u32,
    // ... other fields ... Add more fields as needed
}

#[derive(Debug)]
struct NitfImageHeader {
    image_number: u16,
    image_width: u32,
    image_height: u32,
    // ... other fields ... Add more fields as needed
}

fn parse_null_terminated_string(input: &[u8]) -> IResult<&[u8], String> {
    let (rest, bytes) = take_while1(|b| b != 0)(input)?;
    match str::from_utf8(bytes) {
        Ok(s) => Ok((rest, s.to_string())),
        Err(_) => Err(Err::Error(nom::error::Error{
            input,
            code: ErrorKind::NonNumeric, //Using a valid ErrorKind
        })),
    }
}

fn parse_nitf_file_header(input: &[u8]) -> IResult<&[u8], NitfFileHeader> {
    let (rest, (version, file_header_length)) = tuple((
        parse_null_terminated_string,
        preceded(tag(b"NITF"), be_u32),
    ))(input)?;

    println!("NITF version: {}", version);
    Ok((
        rest,
        NitfFileHeader {
            file_header_length,
            // ... other fields ...
        },
    ))
}

fn parse_nitf_image_header(input: &[u8]) -> IResult<&[u8], NitfImageHeader> {
    let (rest, (image_number, image_width, image_height)) = tuple((be_u16, be_u32, be_u32))(input)?;
    Ok((
        rest,
        NitfImageHeader {
            image_number,
            image_width,
            image_height,
            // ... other fields ...
        },
    ))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <NITF_file>", args[0]);
        return Ok(());
    }

    let filename = &args[1];
    let mut file = File::open(filename)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_nitf_file_header(&buffer) {
        Ok((rest, header)) => {
            println!("File Header: {:?}", header);
            // Process the remaining data (Image Header and image data)
            match parse_nitf_image_header(rest) {
                Ok((_, image_header)) => {
                    println!("Image Header: {:?}", image_header);
                    // ...Further processing for image data and TREs here...
                }
                Err(e) => eprintln!("Error parsing image header: {:?}", e),
            }
        }
        Err(e) => eprintln!("Error parsing file header: {:?}", e),
    }

    Ok(())
}
