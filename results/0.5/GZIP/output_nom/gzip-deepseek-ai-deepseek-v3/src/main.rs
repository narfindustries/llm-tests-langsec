use nom::{
    bytes::complete::{tag, take},
    combinator::map,
    number::complete::{le_u16, le_u32, le_u8},
    sequence::tuple,
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

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
    filename: Option<String>,
    comment: Option<String>,
    hcrc: Option<u16>,
}

#[derive(Debug)]
struct GzipFile {
    header: GzipHeader,
    compressed_data: Vec<u8>,
    crc32: u32,
    isize: u32,
}

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    let (input, (id1, id2, cm, flg, mtime, xfl, os)) = tuple((le_u8, le_u8, le_u8, le_u8, le_u32, le_u8, le_u8))(input)?;

    let (input, extra) = if flg & 0x04 != 0 {
        let (input, xlen) = le_u16(input)?;
        let (input, extra_data) = take(xlen)(input)?;
        (input, Some(extra_data.to_vec()))
    } else {
        (input, None)
    };

    let (input, filename) = if flg & 0x08 != 0 {
        let (input, filename) = map(take_until_null, |s: &[u8]| String::from_utf8_lossy(s).into_owned())(input)?;
        (input, Some(filename))
    } else {
        (input, None)
    };

    let (input, comment) = if flg & 0x10 != 0 {
        let (input, comment) = map(take_until_null, |s: &[u8]| String::from_utf8_lossy(s).into_owned())(input)?;
        (input, Some(comment))
    } else {
        (input, None)
    };

    let (input, hcrc) = if flg & 0x02 != 0 {
        let (input, hcrc) = le_u16(input)?;
        (input, Some(hcrc))
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
            extra,
            filename,
            comment,
            hcrc,
        },
    ))
}

fn take_until_null(input: &[u8]) -> IResult<&[u8], &[u8]> {
    let pos = input.iter().position(|&x| x == 0).unwrap_or(input.len());
    Ok((&input[pos..], &input[..pos]))
}

fn parse_gzip_file(input: &[u8]) -> IResult<&[u8], GzipFile> {
    let (input, header) = parse_gzip_header(input)?;
    let (input, compressed_data) = take(input.len() - 8)(input)?;
    let (input, (crc32, isize)) = tuple((le_u32, le_u32))(input)?;

    Ok((
        input,
        GzipFile {
            header,
            compressed_data: compressed_data.to_vec(),
            crc32,
            isize,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_gzip_file(&buffer) {
        Ok((_, gzip_file)) => {
            println!("{:#?}", gzip_file);
        }
        Err(e) => {
            eprintln!("Failed to parse GZIP file: {:?}", e);
        }
    }
}