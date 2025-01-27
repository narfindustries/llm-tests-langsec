use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32},
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
    modification_time: u32,
    extra_flags: u8,
    os: u8,
    extra_field: Option<Vec<u8>>,
    filename: Option<String>,
    comment: Option<String>,
    crc16: Option<u16>,
}

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    let (input, (id1, id2, compression_method, flags, modification_time, extra_flags, os)) =
        tuple((take(1u8), take(1u8), take(1u8), take(1u8), le_u32, take(1u8), take(1u8)))(input)?;

    let (input, extra_field) = if flags & 0x04 != 0 {
        let (input, xlen) = le_u16(input)?;
        let (input, extra_field) = take(xlen)(input)?;
        (input, Some(extra_field.to_vec()))
    } else {
        (input, None)
    };

    let (input, filename) = if flags & 0x08 != 0 {
        let (input, filename) = take_until_null(input)?;
        (input, Some(String::from_utf8_lossy(filename).into_owned()))
    } else {
        (input, None)
    };

    let (input, comment) = if flags & 0x10 != 0 {
        let (input, comment) = take_until_null(input)?;
        (input, Some(String::from_utf8_lossy(comment).into_owned()))
    } else {
        (input, None)
    };

    let (input, crc16) = if flags & 0x02 != 0 {
        let (input, crc16) = le_u16(input)?;
        (input, Some(crc16))
    } else {
        (input, None)
    };

    Ok((
        input,
        GzipHeader {
            id1: id1[0],
            id2: id2[0],
            compression_method: compression_method[0],
            flags: flags[0],
            modification_time,
            extra_flags: extra_flags[0],
            os: os[0],
            extra_field,
            filename,
            comment,
            crc16,
        },
    ))
}

fn take_until_null(input: &[u8]) -> IResult<&[u8], &[u8]> {
    let mut i = 0;
    while i < input.len() {
        if input[i] == 0 {
            return Ok((&input[i + 1..], &input[0..i]));
        }
        i += 1;
    }
    Err(nom::Err::Error((input, nom::error::ErrorKind::TakeUntil)))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    let result = parse_gzip_header(&buffer);
    match result {
        Ok((_, header)) => println!("{:#?}", header),
        Err(e) => eprintln!("Failed to parse GZIP header: {:?}", e),
    }

    Ok(())
}