use nom::{
    bytes::complete::{take, take_until},
    combinator::map_res,
    number::complete::{be_u16, be_u32},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;
use std::env;

#[derive(Debug)]
struct NITFHeader {
    fhdr: String,
    fver: String,
    clevel: String,
    stype: String,
    odate: String,
    ftitle: String,
    fsclas: String,
    fscop: String,
}

fn parse_nitf_header(input: &[u8]) -> IResult<&[u8], NITFHeader> {
    let (input, fhdr) = map_res(take(9u8), std::str::from_utf8)(input)?;
    let (input, fver) = map_res(take(5u8), std::str::from_utf8)(input)?;
    let (input, clevel) = map_res(take(2u8), std::str::from_utf8)(input)?;
    let (input, stype) = map_res(take(4u8), std::str::from_utf8)(input)?;
    let (input, odate) = map_res(take(14u8), std::str::from_utf8)(input)?;
    let (input, ftitle) = map_res(take(80u8), std::str::from_utf8)(input)?;
    let (input, fsclas) = map_res(take(1u8), std::str::from_utf8)(input)?;
    let (input, fscop) = map_res(take(40u8), std::str::from_utf8)(input)?;

    Ok((
        input,
        NITFHeader {
            fhdr: fhdr.to_string(),
            fver: fver.to_string(),
            clevel: clevel.to_string(),
            stype: stype.to_string(),
            odate: odate.to_string(),
            ftitle: ftitle.trim_end().to_string(),
            fsclas: fsclas.to_string(),
            fscop: fscop.trim_end().to_string(),
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return;
    }
    let file_path = PathBuf::from(&args[1]);
    let mut file = match File::open(file_path) {
        Ok(file) => file,
        Err(e) => {
            eprintln!("Error opening file: {}", e);
            return;
        }
    };

    let mut buffer = Vec::new();
    if let Err(e) = file.read_to_end(&mut buffer) {
        eprintln!("Error reading file: {}", e);
        return;
    }

    match parse_nitf_header(&buffer) {
        Ok((_, header)) => {
            println!("Parsed NITF Header: {:?}", header);
        }
        Err(e) => {
            eprintln!("Error parsing NITF: {:?}", e);
        }
    }
}