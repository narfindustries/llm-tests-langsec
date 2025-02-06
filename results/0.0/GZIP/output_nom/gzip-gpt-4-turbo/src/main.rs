use nom::{
    bytes::complete::{tag, take},
    combinator::{cond, map, map_res},
    number::complete::{le_u16, le_u32, le_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::{
    fs::File,
    io::{self, Read},
    path::PathBuf,
};
use clap::Parser;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Opt {
    #[clap(parse(from_os_str))]
    input_file: PathBuf,
}

#[derive(Debug)]
struct GzipHeader {
    id1: u8,
    id2: u8,
    cm: u8,
    flg: u8,
    mtime: u32,
    xfl: u8,
    os: u8,
    extra: Option<Vec<u8>>,
    fname: Option<String>,
    fcomment: Option<String>,
    fhcrc: Option<u16>,
}

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    map(
        tuple((
            tag([0x1f, 0x8b]), // ID1 and ID2
            le_u8,             // CM
            le_u8,             // FLG
            le_u32,            // MTIME
            le_u8,             // XFL
            le_u8,             // OS
            preceded(le_u8, parse_optional_fields),
        )),
        |(id, cm, flg, mtime, xfl, os, (extra, fname, fcomment, fhcrc))| GzipHeader {
            id1: id[0],
            id2: id[1],
            cm,
            flg,
            mtime,
            xfl,
            os,
            extra,
            fname,
            fcomment,
            fhcrc,
        },
    )(input)
}

fn parse_optional_fields(flg: u8, input: &[u8]) -> IResult<&[u8], (Option<Vec<u8>>, Option<String>, Option<String>, Option<u16>)> {
    let (input, extra) = cond(flg & 0b100 != 0, parse_extra_field)(input)?;
    let (input, fname) = cond(flg & 0b1000 != 0, map_res(parse_zero_terminated_string, String::from_utf8))(input)?;
    let (input, fcomment) = cond(flg & 0b10000 != 0, map_res(parse_zero_terminated_string, String::from_utf8))(input)?;
    let (input, fhcrc) = cond(flg & 0b10 != 0, le_u16)(input)?;
    Ok((input, (extra, fname, fcomment, fhcrc)))
}

fn parse_extra_field(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, len) = le_u16(input)?;
    take(len)(input)
}

fn parse_zero_terminated_string(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let pos = input.iter().position(|&r| r == 0).unwrap_or(input.len());
    let (next_input, result) = input.split_at(pos + 1);
    Ok((next_input, result[..pos].to_vec()))
}

fn main() -> io::Result<()> {
    let opt = Opt::parse();
    let mut file = File::open(opt.input_file)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_gzip_header(&buffer) {
        Ok((_, header)) => {
            println!("{:?}", header);
        }
        Err(e) => {
            eprintln!("Failed to parse GZIP header: {:?}", e);
        }
    }

    Ok(())
}