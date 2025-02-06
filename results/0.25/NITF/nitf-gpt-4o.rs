use nom::{
    bytes::complete::take,
    combinator::map_res,
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct NITFHeader {
    fhdr: String,
    clevel: String,
    stype: String,
    ostaid: String,
    fdt: String,
    ftitle: String,
    fsclas: String,
    // Add more fields as needed
}

fn parse_string(input: &[u8], length: usize) -> IResult<&[u8], String> {
    map_res(take(length), |bytes: &[u8]| {
        std::str::from_utf8(bytes).map(|s| s.to_string())
    })(input)
}

fn parse_nitf_header(input: &[u8]) -> IResult<&[u8], NITFHeader> {
    let (input, (fhdr, clevel, stype, ostaid, fdt, ftitle, fsclas)) = tuple((
        |i| parse_string(i, 9),  // FHDR
        |i| parse_string(i, 2),  // CLEVEL
        |i| parse_string(i, 4),  // STYPE
        |i| parse_string(i, 10), // OSTAID
        |i| parse_string(i, 14), // FDT
        |i| parse_string(i, 80), // FTITLE
        |i| parse_string(i, 1),  // FSCLAS
        // Add more fields as needed
    ))(input)?;

    Ok((
        input,
        NITFHeader {
            fhdr,
            clevel,
            stype,
            ostaid,
            fdt,
            ftitle,
            fsclas,
            // Add more fields as needed
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        return Ok(());
    }

    let filename = &args[1];
    let mut file = File::open(filename)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_nitf_header(&buffer) {
        Ok((_, header)) => {
            println!("Parsed NITF Header: {:?}", header);
        }
        Err(e) => {
            eprintln!("Failed to parse NITF file: {:?}", e);
        }
    }

    Ok(())
}