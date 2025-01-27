use std::env;
use std::fs::File;
use std::io::Read;

use nom::bits::bits;
use nom::bytes::complete::{take, take_while_m_n};
use nom::combinator::{map, verify};
use nom::error::{Error, ErrorKind};
use nom::multi::take_while_m_n;
use nom::number::complete::{be_u16, be_u32};
use nom::sequence::tuple;
use nom::IResult;

#[derive(Debug)]
struct GzipHeader {
    id1: u8,
    id2: u8,
    cm: u8,
    flags: u8,
    mtime: u32,
    xfl: u8,
    os: u8,
}

#[derive(Debug)]
struct GzipFooter {
    crc16: u32,
    isize: u32,
}

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    map(
        tuple((
            take(2usize),
            take(1usize),
            take(1usize),
            take(1usize),
            be_u32,
            take(1usize),
            take(1usize),
        )),
        |(id, cm, flags, mtime, xfl, os)| GzipHeader {
            id1: id[0],
            id2: id[1],
            cm: cm[0],
            flags: flags[0],
            mtime: mtime,
            xfl: xfl[0],
            os: os[0],
        },
    )(input)
}

fn parse_gzip_footer(input: &[u8]) -> IResult<&[u8], GzipFooter> {
    map(
        tuple((be_u32, be_u32)),
        |(crc16, isize)| GzipFooter { crc16, isize },
    )(input)
}

fn parse_gzip(input: &[u8]) -> IResult<&[u8], (GzipHeader, GzipFooter)> {
    let (input, header) = parse_gzip_header(input)?;
    let (input, _) = take_while_m_n(0, 65535, |x| x != 0)(input)?;
    let (input, footer) = parse_gzip_footer(input)?;
    Ok((input, (header, footer)))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return;
    }
    let mut file = File::open(&args[1]).unwrap();
    let mut input = Vec::new();
    file.read_to_end(&mut input).unwrap();
    match parse_gzip(&input) {
        Ok((remaining, (header, footer))) => {
            println!("Header: {:?}", header);
            println!("Footer: {:?}", footer);
            assert_eq!(remaining.len(), 0);
        }
        Err(e) => println!("Error: {:?}", e),
    }
}