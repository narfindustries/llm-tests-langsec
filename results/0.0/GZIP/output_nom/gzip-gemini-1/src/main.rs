use nom::{
    bytes::complete::{tag, take, take_while_m_n},
    combinator::{map, map_res, opt},
    number::complete::{be_u16, be_u32},
    sequence::tuple,
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::process;
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
}

#[derive(Debug)]
struct GzipExtra {
    subfield_id: u16,
    subfield_len: u16,
    subfield_data: Vec<u8>,
}

#[derive(Debug)]
struct GzipFile {
    header: GzipHeader,
    extra: Option<Vec<GzipExtra>>,
    filename: Option<String>,
    comment: Option<String>,
    header_crc: Option<u16>,
    compressed_data: Vec<u8>,
    crc32: u32,
    isize: u32,
}

fn gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    map(
        tuple((
            tag(&b"\x1f\x8b"[..]),
            take(1u8),
            take(1u8),
            take(4u8),
            take(1u8),
            take(1u8),
        )),
        |(id1_id2, cm, flg, mtime, xfl, os)| GzipHeader {
            id1: id1_id2[0],
            id2: id1_id2[1],
            cm: cm[0],
            flg: flg[0],
            mtime: u32::from_be_bytes(mtime.try_into().unwrap()),
            xfl: xfl[0],
            os: os[0],
        },
    )(input)
}

fn gzip_extra(input: &[u8]) -> IResult<&[u8], GzipExtra> {
    map(
        tuple((be_u16, be_u16, take)),
        |(subfield_id, subfield_len, subfield_data)| GzipExtra {
            subfield_id,
            subfield_len,
            subfield_data: subfield_data.to_vec(),
        },
    )(input)
}

fn gzip_string(input: &[u8]) -> IResult<&[u8], Option<String>> {
    map_res(
        opt(map(take_while_m_n(1, 255, |b| *b != 0), |bytes| str::from_utf8(bytes))),
        |s| s.transpose(),
    )(input)
}

fn gzip_file(input: &[u8]) -> IResult<&[u8], GzipFile> {
    let (input, header) = gzip_header(input)?;
    let (input, extra) = if (header.flg & 0b00000100) != 0 {
        let (input, len) = be_u16(input)?;
        let (input, extra_data) = take(len as usize)(input)?;
        let mut extra_fields = Vec::new();
        let mut remaining_data = extra_data;
        while remaining_data.len() > 0 {
            let (rem, extra_field) = gzip_extra(remaining_data)?;
            extra_fields.push(extra_field);
            remaining_data = rem;
        }
        (input, Some(extra_fields))
    } else {
        (input, None)
    };
    let (input, filename) = gzip_string(input)?;
    let (input, comment) = gzip_string(input)?;
    let (input, header_crc) = if (header.flg & 0b00000010) != 0 {
        map(be_u16, |crc| Some(crc))(input)?
    } else {
        (input, None)
    };
    let (input, compressed_data) = take_until_trailer(input)?;
    let (input, crc32) = be_u32(input)?;
    let (input, isize) = be_u32(input)?;
    Ok((
        input,
        GzipFile {
            header,
            extra,
            filename,
            comment,
            header_crc,
            compressed_data: compressed_data.to_vec(),
            crc32,
            isize,
        },
    ))
}

fn take_until_trailer(input: &[u8]) -> IResult<&[u8], &[u8]> {
    let len = input.len() - 8;
    take(len)(input)
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <gzip_file>", args[0]);
        process::exit(1);
    }

    let filename = &args[1];
    let mut file = match File::open(filename) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Error opening file {}: {}", filename, err);
            process::exit(1);
        }
    };

    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Ok(_) => (),
        Err(err) => {
            eprintln!("Error reading file {}: {}", filename, err);
            process::exit(1);
        }
    };

    match gzip_file(&buffer) {
        Ok((_, gzip_file)) => println!("{:#?}", gzip_file),
        Err(err) => {
            eprintln!("Error parsing gzip file: {:?}", err);
            process::exit(1);
        }
    }
}