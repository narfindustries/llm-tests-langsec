use nom::{
    bytes::complete::{tag, take, take_until},
    combinator::{cond, map_res},
    number::complete::{le_u16, le_u32, le_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::env;
use std::str;

#[derive(Debug)]
struct GzipHeader {
    id1: u8,
    id2: u8,
    cm: u8,
    flg: u8,
    mtime: u32,
    xfl: u8,
    os: u8,
    fextra: Option<Vec<u8>>,
    fname: Option<String>,
    fcomment: Option<String>,
    fhcrc: Option<u16>,
}

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    let (input, (id1, id2, cm, flg, mtime, xfl, os)) = tuple((tag(&[0x1f, 0x8b]), le_u8, le_u8, le_u32, le_u8, le_u8))(input)?;

    let flag_bits = flg;
    let has_fextra = (flag_bits & 0b00000100) != 0;
    let has_fname = (flag_bits & 0b00001000) != 0;
    let has_fcomment = (flag_bits & 0b00010000) != 0;
    let has_fhcrc = (flag_bits & 0b00000010) != 0;

    let (input, fextra) = cond(
        has_fextra,
        preceded(le_u16, take),
    )(input)?;

    let (input, fname) = cond(
        has_fname,
        map_res(take_until("\0"), str::from_utf8),
    )(input)?;
    let (input, _) = cond(has_fname, take(1usize))(input);  // consume the null terminator

    let (input, fcomment) = cond(
        has_fcomment,
        map_res(take_until("\0"), str::from_utf8),
    )(input)?;
    let (input, _) = cond(has_fcomment, take(1usize))(input);  // consume the null terminator

    let (input, fhcrc) = cond(has_fhcrc, le_u16)(input)?;

    Ok((
        input,
        GzipHeader {
            id1,
            id2,
            cm,
            flg,
            mtime,
            xfl,
            os,
            fextra: fextra.map(|x| x.to_vec()),
            fname: fname.map(|s| s.to_string()),
            fcomment: fcomment.map(|s| s.to_string()),
            fhcrc,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        return Err(io::Error::new(io::ErrorKind::InvalidInput, "Usage: gzip_parser <file_path>"));
    }
    let file_path = &args[1];
    let mut file = File::open(file_path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_gzip_header(&buffer) {
        Ok((_, header)) => println!("{:#?}", header),
        Err(e) => println!("Failed to parse GZIP header: {:?}", e),
    }

    Ok(())
}