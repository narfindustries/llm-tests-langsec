use nom::{
    bytes::complete::{tag, take},
    multi::{count, many0, many_m_n},
    number::complete::{be_u8, be_u16, be_u32},
    sequence::{tuple, preceded},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct NitfHeader {
    fhdr: String,
    fver: String,
    clevel: u8,
    stype: String,
    ostaid: String,
    fdt: String,
    ftitle: Option<String>,
    fscop: u8,
    fscpys: u8,
}

#[derive(Debug)]
struct ImageSubheader {
    itype: String,
    irep: String,
    icords: String,
    igeolo: String,
    nicom: u8,
}

fn parse_nitf_header(input: &[u8]) -> IResult<&[u8], NitfHeader> {
    let (input, fhdr) = take(4usize)(input)?;
    let (input, fver) = take(5usize)(input)?;
    let (input, clevel) = be_u8(input)?;
    let (input, stype) = take(10usize)(input)?;
    let (input, ostaid) = take(10usize)(input)?;
    let (input, fdt) = take(14usize)(input)?;
    let (input, ftitle) = take(80usize)(input)?;
    let (input, fscop) = be_u8(input)?;
    let (input, fscpys) = be_u8(input)?;

    Ok((input, NitfHeader {
        fhdr: String::from_utf8_lossy(fhdr).into(),
        fver: String::from_utf8_lossy(fver).into(),
        clevel,
        stype: String::from_utf8_lossy(stype).into(),
        ostaid: String::from_utf8_lossy(ostaid).into(),
        fdt: String::from_utf8_lossy(fdt).into(),
        ftitle: if ftitle.iter().all(|&x| x == b' ') { None } else { Some(String::from_utf8_lossy(ftitle).into()) },
        fscop,
        fscpys,
    }))
}

fn parse_image_subheader(input: &[u8]) -> IResult<&[u8], ImageSubheader> {
    let (input, itype) = take(10usize)(input)?;
    let (input, irep) = take(20usize)(input)?;
    let (input, icords) = take(2usize)(input)?;
    let (input, igeolo) = take(60usize)(input)?;
    let (input, nicom) = be_u8(input)?;

    Ok((input, ImageSubheader {
        itype: String::from_utf8_lossy(itype).into(),
        irep: String::from_utf8_lossy(irep).into(),
        icords: String::from_utf8_lossy(icords).into(),
        igeolo: String::from_utf8_lossy(igeolo).into(),
        nicom,
    }))
}

fn parse_nitf(input: &[u8]) -> IResult<&[u8], (NitfHeader, Vec<ImageSubheader>)> {
    let (input, header) = parse_nitf_header(input)?;
    let (input, image_subheaders) = many_m_n(1, 10, parse_image_subheader)(input)?;

    Ok((input, (header, image_subheaders)))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <nitf_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_nitf(&buffer) {
        Ok((_, (header, image_subheaders))) => {
            println!("NITF Header: {:?}", header);
            println!("Image Subheaders: {:?}", image_subheaders);
        }
        Err(e) => eprintln!("Parsing error: {:?}", e),
    }

    Ok(())
}