use nom::{
    bytes::complete::{tag, take},
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
    compression_method: u8,
    flags: u8,
    mtime: u32,
    extra_flags: u8,
    os: u8,
    extra: Option<Vec<u8>>,
    original_filename: Option<String>,
    comment: Option<String>,
    crc16: Option<u16>,
}

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    let (input, (id1, id2, compression_method, flags, mtime, extra_flags, os)) = tuple((
        tag(&[0x1f, 0x8b]), // ID1 and ID2
        le_u8,
        le_u8,
        le_u8,
        le_u32,
        le_u8,
        le_u8,
    ))(input)?;

    let (mut input, mut extra) = (input, None);
    if flags & 0x04 != 0 {
        let (i, extra_len) = le_u16(input)?;
        let (i, extra_data) = take(extra_len)(i)?;
        input = i;
        extra = Some(extra_data.to_vec());
    }

    let (mut input, mut original_filename) = (input, None);
    if flags & 0x08 != 0 {
        let (i, filename) = take_until_0(input)?;
        input = i;
        original_filename = Some(String::from_utf8_lossy(filename).to_string());
    }

    let (mut input, mut comment) = (input, None);
    if flags & 0x10 != 0 {
        let (i, com) = take_until_0(input)?;
        input = i;
        comment = Some(String::from_utf8_lossy(com).to_string());
    }

    let (input, crc16) = if flags & 0x02 != 0 {
        let (i, crc) = le_u16(input)?;
        (i, Some(crc))
    } else {
        (input, None)
    };

    Ok((
        input,
        GzipHeader {
            id1,
            id2,
            compression_method,
            flags,
            mtime,
            extra_flags,
            os,
            extra,
            original_filename,
            comment,
            crc16,
        },
    ))
}

fn take_until_0(input: &[u8]) -> IResult<&[u8], &[u8]> {
    let pos = input.iter().position(|&r| r == 0x00).unwrap_or(input.len());
    let (i, result) = input.split_at(pos);
    Ok((i, result))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "Usage: gzip_parser <file_path>",
        ));
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_gzip_header(&buffer) {
        Ok((_, header)) => println!("{:?}", header),
        Err(e) => println!("Failed to parse GZIP header: {:?}", e),
    }

    Ok(())
}