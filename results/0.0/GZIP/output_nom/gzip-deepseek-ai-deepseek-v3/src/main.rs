use nom::{
    bytes::complete::{take, take_until},
    combinator::map_res,
    number::complete::{le_u16, le_u32, le_u8},
    sequence::tuple,
    IResult,
};
use std::{
    env,
    fs::File,
    io::{self, Read},
};

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
    header_crc16: Option<u16>,
}

#[derive(Debug)]
struct GzipFile {
    header: GzipHeader,
    compressed_data: Vec<u8>,
    crc32: u32,
    isize: u32,
}

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    let (input, (id1, id2, cm, flg, mtime, xfl, os)) =
        tuple((le_u8, le_u8, le_u8, le_u8, le_u32, le_u8, le_u8))(input)?;

    let (input, extra) = if flg & 0x04 != 0 {
        let (input, xlen) = le_u16(input)?;
        let (input, extra_data) = take(xlen)(input)?;
        (input, Some(extra_data.to_vec()))
    } else {
        (input, None)
    };

    let (input, filename) = if flg & 0x08 != 0 {
        let (input, filename_data) = map_res(take_until(&[0][..]), |s: &[u8]| {
            String::from_utf8(s.to_vec())
        })(input)?;
        let (input, _) = take(1usize)(input)?; // Consume the null terminator
        (input, Some(filename_data))
    } else {
        (input, None)
    };

    let (input, comment) = if flg & 0x10 != 0 {
        let (input, comment_data) = map_res(take_until(&[0][..]), |s: &[u8]| {
            String::from_utf8(s.to_vec())
        })(input)?;
        let (input, _) = take(1usize)(input)?; // Consume the null terminator
        (input, Some(comment_data))
    } else {
        (input, None)
    };

    let (input, header_crc16) = if flg & 0x02 != 0 {
        let (input, crc) = le_u16(input)?;
        (input, Some(crc))
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
            header_crc16,
        },
    ))
}

fn parse_gzip_file(input: &[u8]) -> IResult<&[u8], GzipFile> {
    let (input, header) = parse_gzip_header(input)?;
    let (input, compressed_data) = take(input.len() - 8)(input)?;
    let (input, crc32) = le_u32(input)?;
    let (input, isize) = le_u32(input)?;

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

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <gzip_file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_gzip_file(&buffer) {
        Ok((_, gzip_file)) => {
            println!("{:#?}", gzip_file);
        }
        Err(e) => {
            eprintln!("Failed to parse GZIP file: {:?}", e);
        }
    }

    Ok(())
}