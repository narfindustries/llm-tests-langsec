use nom::{
    bytes::complete::{tag, take, take_while},
    combinator::{map, opt},
    multi::{count, many0},
    number::complete::{le_u8, le_u16, le_u32},
    sequence::{tuple, preceded},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct NitfHeader {
    fhdr: String,
    clevel: u8,
    stype: String,
    ostaid: String,
    fdt: String,
    ftitle: Option<String>,
    fscop: String,
    fscpys: String,
    image_segments: Vec<ImageSegment>,
}

#[derive(Debug)]
struct ImageSegment {
    itype: String,
    irep: String,
    icat: String,
    isub: String,
    idlvl: u16,
    isclas: String,
    iscode: Option<String>,
    isctro: Option<String>,
    idata: Vec<u8>,
}

fn parse_nitf_header(input: &[u8]) -> IResult<&[u8], NitfHeader> {
    let (input, fhdr) = map(take(9usize), |bytes: &[u8]| String::from_utf8_lossy(bytes).to_string())(input)?;
    let (input, clevel) = le_u8(input)?;
    let (input, stype) = map(take(1usize), |bytes: &[u8]| String::from_utf8_lossy(bytes).to_string())(input)?;
    let (input, ostaid) = map(take(10usize), |bytes: &[u8]| String::from_utf8_lossy(bytes).to_string())(input)?;
    let (input, fdt) = map(take(14usize), |bytes: &[u8]| String::from_utf8_lossy(bytes).to_string())(input)?;
    let (input, ftitle) = opt(map(take(80usize), |bytes: &[u8]| String::from_utf8_lossy(bytes).to_string()))(input)?;
    let (input, fscop) = map(take(2usize), |bytes: &[u8]| String::from_utf8_lossy(bytes).to_string())(input)?;
    let (input, fscpys) = map(take(11usize), |bytes: &[u8]| String::from_utf8_lossy(bytes).to_string())(input)?;
    
    let (input, image_segments) = many0(parse_image_segment)(input)?;

    Ok((input, NitfHeader {
        fhdr,
        clevel,
        stype,
        ostaid,
        fdt,
        ftitle,
        fscop,
        fscpys,
        image_segments,
    }))
}

fn parse_image_segment(input: &[u8]) -> IResult<&[u8], ImageSegment> {
    let (input, itype) = map(take(10usize), |bytes: &[u8]| String::from_utf8_lossy(bytes).to_string())(input)?;
    let (input, irep) = map(take(8usize), |bytes: &[u8]| String::from_utf8_lossy(bytes).to_string())(input)?;
    let (input, icat) = map(take(3usize), |bytes: &[u8]| String::from_utf8_lossy(bytes).to_string())(input)?;
    let (input, isub) = map(take(10usize), |bytes: &[u8]| String::from_utf8_lossy(bytes).to_string())(input)?;
    let (input, idlvl) = le_u16(input)?;
    let (input, isclas) = map(take(1usize), |bytes: &[u8]| String::from_utf8_lossy(bytes).to_string())(input)?;
    let (input, iscode) = opt(map(take(11usize), |bytes: &[u8]| String::from_utf8_lossy(bytes).to_string()))(input)?;
    let (input, isctro) = opt(map(take(11usize), |bytes: &[u8]| String::from_utf8_lossy(bytes).to_string()))(input)?;
    
    // Placeholder for image data parsing - you'd need to implement actual image data parsing
    let (input, idata) = take(1024usize)(input)?;

    Ok((input, ImageSegment {
        itype,
        irep,
        icat,
        isub,
        idlvl,
        isclas,
        iscode,
        isctro,
        idata: idata.to_vec(),
    }))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <nitf_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_nitf_header(&buffer) {
        Ok((_, nitf_header)) => {
            println!("Parsed NITF Header: {:?}", nitf_header);
            Ok(())
        }
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1);
        }
    }
}