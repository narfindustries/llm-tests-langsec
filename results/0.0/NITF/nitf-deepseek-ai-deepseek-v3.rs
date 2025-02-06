use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u16, be_u32, be_u64},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs;

#[derive(Debug)]
struct NITFHeader {
    fhdr: String,
    fver: String,
    clevel: String,
    stype: String,
    ostaid: String,
    fdt: String,
    ftitle: String,
    fl: u64,
    numi: u16,
    nums: u16,
    numl: u16,
    numt: u16,
    numdes: u16,
    numres: u16,
}

fn parse_nitf_header(input: &[u8]) -> IResult<&[u8], NITFHeader> {
    let (input, (fhdr, fver, clevel, stype, ostaid, fdt, ftitle, fl, numi, nums, numl, numt, numdes, numres)) = tuple((
        take(4usize),
        take(4usize),
        take(2usize),
        take(4usize),
        take(10usize),
        take(14usize),
        take(80usize),
        be_u64,
        be_u16,
        be_u16,
        be_u16,
        be_u16,
        be_u16,
        be_u16,
    ))(input)?;

    Ok((
        input,
        NITFHeader {
            fhdr: String::from_utf8_lossy(fhdr).to_string(),
            fver: String::from_utf8_lossy(fver).to_string(),
            clevel: String::from_utf8_lossy(clevel).to_string(),
            stype: String::from_utf8_lossy(stype).to_string(),
            ostaid: String::from_utf8_lossy(ostaid).to_string(),
            fdt: String::from_utf8_lossy(fdt).to_string(),
            ftitle: String::from_utf8_lossy(ftitle).to_string(),
            fl,
            numi,
            nums,
            numl,
            numt,
            numdes,
            numres,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <NITF file>", args[0]);
        return;
    }

    let file_path = &args[1];
    let data = fs::read(file_path).expect("Failed to read file");

    match parse_nitf_header(&data) {
        Ok((_, header)) => println!("{:?}", header),
        Err(e) => eprintln!("Failed to parse NITF header: {:?}", e),
    }
}