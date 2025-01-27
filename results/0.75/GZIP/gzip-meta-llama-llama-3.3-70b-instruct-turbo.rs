use nom::{
    bits::complete::take,
    bytes::complete::{tag, take_till, take_while_m_n},
    combinator::{map, verify},
    multi::take,
    IResult,
};
use std::{
    env,
    fs::File,
    io::{Read, Seek, SeekFrom},
};

// Define a GZIP trailer structure
#[derive(Debug)]
struct GzipTrailer {
    crc32: u32,
    isize: u32,
}

// Define a GZIP file structure
#[derive(Debug)]
struct GzipFile {
    header: GzipHeader,
    body: Vec<u8>,
    trailer: GzipTrailer,
}

// Define a GZIP header structure
#[derive(Debug)]
struct GzipHeader {
    id1: u8,
    id2: u8,
    cm: u8,
    flags: u8,
    mtime: u32,
    xfl: u8,
    os: u8,
    fname: Vec<u8>,
    comment: Vec<u8>,
    hcrc: Option<u16>,
}

fn parse_id1(input: &[u8]) -> IResult<&[u8], u8> {
    verify(tag([0x1f]), |x| *x == 0x1f)(input)
}

fn parse_id2(input: &[u8]) -> IResult<&[u8], u8> {
    verify(tag([0x8b]), |x| *x == 0x8b)(input)
}

fn parse_cm(input: &[u8]) -> IResult<&[u8], u8> {
    verify(tag([0x08]), |x| *x == 0x08)(input)
}

fn parse_flags(input: &[u8]) -> IResult<&[u8], u8> {
    take(1usize)(input)
}

fn parse_mtime(input: &[u8]) -> IResult<&[u8], u32> {
    map(take(4usize), |mtime: &[u8]| {
        ((mtime[0] as u32) << 24) | ((mtime[1] as u32) << 16) | ((mtime[2] as u32) << 8) | (mtime[3] as u32)
    })(input)
}

fn parse_xfl(input: &[u8]) -> IResult<&[u8], u8> {
    take(1usize)(input)
}

fn parse_os(input: &[u8]) -> IResult<&[u8], u8> {
    take(1usize)(input)
}

fn parse_fname(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    take_while_m_n(0, 65535, |x| x != 0)(input)
}

fn parse_comment(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    take_while_m_n(0, 65535, |x| x != 0)(input)
}

fn parse_hcrc(input: &[u8]) -> IResult<&[u8], Option<u16>> {
    let (input, present) = verify(tag([0x00]), |x| *x != 0x00)(input).unwrap_or((input, false));
    if present {
        map(take(2usize), |hcrc: &[u8]| {
            ((hcrc[0] as u16) << 8) | (hcrc[1] as u16)
        })(input)
        .map(|(input, hcrc)| (input, Some(hcrc)))
    } else {
        Ok((input, None))
    }
}

fn parse_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    let (input, _) = parse_id1(input)?;
    let (input, _) = parse_id2(input)?;
    let (input, cm) = parse_cm(input)?;
    let (input, flags) = parse_flags(input)?;
    let (input, mtime) = parse_mtime(input)?;
    let (input, xfl) = parse_xfl(input)?;
    let (input, os) = parse_os(input)?;
    let (input, fname) = parse_fname(input)?;
    let (input, comment) = parse_comment(input)?;
    let (input, hcrc) = parse_hcrc(input)?;
    Ok((input, GzipHeader { id1: 0x1f, id2: 0x8b, cm, flags, mtime, xfl, os, fname, comment, hcrc }))
}

fn parse_body(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    take_till(|x| x == 0x00)(input)
}

fn parse_trailer(input: &[u8]) -> IResult<&[u8], GzipTrailer> {
    let (input, crc32) = map(take(4usize), |crc32: &[u8]| {
        ((crc32[0] as u32) << 24) | ((crc32[1] as u32) << 16) | ((crc32[2] as u32) << 8) | (crc32[3] as u32)
    })(input)?;
    let (input, isize) = map(take(4usize), |isize: &[u8]| {
        ((isize[0] as u32) << 24) | ((isize[1] as u32) << 16) | ((isize[2] as u32) << 8) | (isize[3] as u32)
    })(input)?;
    Ok((input, GzipTrailer { crc32, isize }))
}

fn parse_file(input: &[u8]) -> IResult<&[u8], GzipFile> {
    let (input, header) = parse_header(input)?;
    let (input, body) = parse_body(input)?;
    let (input, trailer) = parse_trailer(input)?;
    Ok((input, GzipFile { header, body, trailer }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <input_file>", args[0]);
    }
    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut data = Vec::new();
    file.read_to_end(&mut data).expect("Failed to read file");
    let (_rest, file) = parse_file(&data).unwrap();
    println!("{:?}", file);
}