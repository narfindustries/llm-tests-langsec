use nom::{
    bytes::complete::{take, take_while_m_n},
    combinator::{map_res, opt},
    multi::count,
    number::complete::{be_u16, be_u32},
    sequence::tuple,
    IResult,
};
use std::fs::File;
use std::io::{self, Read};

/// Parse a fixed-length ASCII field
fn parse_fixed_length_ascii(input: &[u8], length: usize) -> IResult<&[u8], String> {
    map_res(take(length), |bytes: &[u8]| {
        std::str::from_utf8(bytes).map(|s| s.trim_end().to_string())
    })(input)
}

/// Parse a NITF header
fn parse_nitf_header(input: &[u8]) -> IResult<&[u8], NITFHeader> {
    let (input, (fhdr, fver, clevel, stype, osta_id, ftitle, fsclas)) = tuple((
        parse_fixed_length_ascii,
        parse_fixed_length_ascii,
        parse_fixed_length_ascii,
        parse_fixed_length_ascii,
        parse_fixed_length_ascii,
        parse_fixed_length_ascii,
        parse_fixed_length_ascii,
    ))(input)?;

    Ok((
        input,
        NITFHeader {
            fhdr,
            fver,
            clevel,
            stype,
            osta_id,
            ftitle,
            fsclas,
        },
    ))
}

/// Definition of the NITF header structure
#[derive(Debug)]
struct NITFHeader {
    fhdr: String,
    fver: String,
    clevel: String,
    stype: String,
    osta_id: String,
    ftitle: String,
    fsclas: String,
}

/// Main function to parse NITF files
fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = match File::open(filename) {
        Ok(file) => file,
        Err(error) => {
            eprintln!("Failed to open file {}: {}", filename, error);
            return;
        },
    };

    let mut buffer = Vec::new();
    if let Err(error) = file.read_to_end(&mut buffer) {
        eprintln!("Failed to read file {}: {}", filename, error);
        return;
    }

    match parse_nitf_header(&buffer) {
        Ok((_, header)) => {
            println!("Parsed NITF Header: {:?}", header);
        },
        Err(error) => {
            eprintln!("Failed to parse NITF header: {:?}", error);
        },
    }
}