use nom::{
    bytes::complete::take,
    combinator::map_res,
    number::complete::be_u32,
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct NITFFileHeader {
    fhdr: String,
    clevel: u8,
    stype: String,
    ostaid: String,
    fdt: String,
    ftitle: String,
    fsclas: char,
    fscode: String,
    fsctlh: String,
    fsrel: String,
    fsdctp: String,
    fsdcdt: String,
    fsdcxm: String,
    oname: String,
    ophone: String,
    fl: u32,
    hl: u32,
}

fn parse_string(input: &[u8], length: usize) -> IResult<&[u8], String> {
    map_res(take(length), |bytes: &[u8]| {
        std::str::from_utf8(bytes).map(|s| s.trim().to_string())
    })(input)
}

fn parse_char(input: &[u8]) -> IResult<&[u8], char> {
    map_res(take(1usize), |bytes: &[u8]| {
        std::str::from_utf8(bytes).map(|s| s.chars().next().unwrap())
    })(input)
}

fn parse_nitf_file_header(input: &[u8]) -> IResult<&[u8], NITFFileHeader> {
    let (input, (fhdr, clevel, stype, ostaid, fdt, ftitle, fsclas, fscode, fsctlh, fsrel, fsdctp, fsdcdt, fsdcxm, oname, ophone, fl, hl)) =
        tuple((
            |i| parse_string(i, 9),  // FHDR
            map_res(take(2usize), |bytes: &[u8]| {
                std::str::from_utf8(bytes).map(|s| s.parse::<u8>().unwrap())
            }),  // CLEVEL
            |i| parse_string(i, 4),  // STYPE
            |i| parse_string(i, 10), // OSTAID
            |i| parse_string(i, 14), // FDT
            |i| parse_string(i, 80), // FTITLE
            parse_char,              // FSCLAS
            |i| parse_string(i, 40), // FSCODE
            |i| parse_string(i, 40), // FSCTLH
            |i| parse_string(i, 40), // FSREL
            |i| parse_string(i, 40), // FSDCTP
            |i| parse_string(i, 8),  // FSDCDT
            |i| parse_string(i, 40), // FSDCXM
            |i| parse_string(i, 27), // ONAME
            |i| parse_string(i, 18), // OPHONE
            be_u32,                  // FL
            be_u32,                  // HL
        ))(input)?;

    Ok((
        input,
        NITFFileHeader {
            fhdr,
            clevel,
            stype,
            ostaid,
            fdt,
            ftitle,
            fsclas,
            fscode,
            fsctlh,
            fsrel,
            fsdctp,
            fsdcdt,
            fsdcxm,
            oname,
            ophone,
            fl,
            hl,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return Ok(());
    }

    let filename = &args[1];
    let mut file = File::open(filename)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_nitf_file_header(&buffer) {
        Ok((_, header)) => println!("{:?}", header),
        Err(e) => eprintln!("Failed to parse NITF file: {:?}", e),
    }

    Ok(())
}