use std::fs::File;
use std::io::Read;
use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::length_data,
    number::complete::{le_u16, le_u32, u8},
    sequence::tuple,
    IResult,
};
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
    xlen: Option<u16>,
    extra: Option<Vec<u8>>,
    fname: Option<String>,
    fcomment: Option<String>,
    hcrc: Option<u16>,
}

#[derive(Debug)]
struct GzipFooter {
    crc32: u32,
    isize: u32,
}

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    let (input, (id1, id2, cm, flg, mtime, xfl, os)) = tuple((u8, u8, u8, u8, le_u32, u8, u8))(input)?;
    let (input, xlen) = if flg & 0x04 != 0 {
        map(le_u16, Some)(input)?
    } else {
        (input, None)
    };
    let (input, extra) = if let Some(len) = xlen {
        map(take(len as usize), |v: &[u8]| Some(v.to_vec()))(input)?
    } else {
        (input, None)
    };
    let (input, fname) = if flg & 0x08 != 0 {
        map(map_res(length_data(u8), |v: &[u8]| str::from_utf8(v).map(|s| s.to_string())), Some)(input)?
    } else {
        (input, None)
    };
    let (input, fcomment) = if flg & 0x10 != 0 {
        map(map_res(length_data(u8), |v: &[u8]| str::from_utf8(v).map(|s| s.to_string())), Some)(input)?
    } else {
        (input, None)
    };
    let (input, hcrc) = if flg & 0x02 != 0 {
        map(le_u16, Some)(input)?
    } else {
        (input, None)
    };
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
            xlen,
            extra,
            fname,
            fcomment,
            hcrc,
        },
    ))
}

fn parse_gzip_footer(input: &[u8]) -> IResult<&[u8], GzipFooter> {
    let (input, (crc32, isize)) = tuple((le_u32, le_u32))(input)?;
    Ok((input, GzipFooter { crc32, isize }))
}

fn parse_gzip(input: &[u8]) -> IResult<&[u8], (GzipHeader, Vec<u8>, GzipFooter)> {
    let (input, header) = parse_gzip_header(input)?;
    let compressed_data_end = input.len() - 8;
    let (input, compressed_data) = take(compressed_data_end)(input)?;
    let (input, footer) = parse_gzip_footer(input)?;
    Ok((input, (header, compressed_data.to_vec(), footer)))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_gzip(&buffer) {
        Ok((_, (header, compressed_data, footer))) => {
            println!("Header: {:?}", header);
            println!("Compressed Data Length: {}", compressed_data.len());
            println!("Footer: {:?}", footer);
        }
        Err(e) => eprintln!("Failed to parse GZIP file: {:?}", e),
    }
}