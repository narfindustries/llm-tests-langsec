use std::fs::File;
use std::io::Read;
use std::path::Path;
use nom::bytes::complete::{tag, take, take_until};
use nom::number::complete::{le_u16, le_u32, u8};
use nom::sequence::{tuple, terminated};
use nom::IResult;

#[derive(Debug)]
struct GzipHeader {
    cm: u8,
    flags: u8,
    mtime: u32,
    xfl: u8,
    os: u8,
    extra: Option<Vec<u8>>,
    filename: Option<String>,
    comment: Option<String>,
    crc16: Option<u16>,
}

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    let (input, (_, cm, flags, mtime, xfl, os)) = tuple((tag([0x1F, 0x8B]), u8, u8, le_u32, u8, u8))(input)?;

    let mut remaining_flags = flags;
    let (input, extra) = if remaining_flags & 0x04 != 0 {
        let (input, xlen) = le_u16(input)?;
        let (input, extra_field) = take(xlen as usize)(input)?;
        remaining_flags &= !0x04;
        (input, Some(extra_field.to_vec()))
    } else {
        (input, None)
    };

    let (input, filename) = if remaining_flags & 0x08 != 0 {
        let (input, filename_bytes) = terminated(take_until(&b"\0"[..]), tag(b"\0"))(input)?;
        remaining_flags &= !0x08;
        let filename_string = String::from_utf8_lossy(filename_bytes).into_owned();
        (input, Some(filename_string))
    } else {
        (input, None)
    };

    let (input, comment) = if remaining_flags & 0x10 != 0 {
        let (input, comment_bytes) = terminated(take_until(&b"\0"[..]), tag(b"\0"))(input)?;
        remaining_flags &= !0x10;
        let comment_string = String::from_utf8_lossy(comment_bytes).into_owned();
        (input, Some(comment_string))
    } else {
        (input, None)
    };

    let (input, crc16) = if remaining_flags & 0x02 != 0 {
        let (input, crc) = le_u16(input)?;
        (input, Some(crc))
    } else {
        (input, None)
    };

    Ok((
        input,
        GzipHeader {
            cm,
            flags,
            mtime,
            xfl,
            os,
            extra,
            filename,
            comment,
            crc16,
        },
    ))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <path_to_gzip_file>", args[0]);
        std::process::exit(1);
    }

    let file_path = &args[1];
    if !Path::new(file_path).exists() {
        eprintln!("File not found: {}", file_path);
        std::process::exit(1);
    }

    let mut file = File::open(file_path).expect("Failed to open the file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read the file");

    match parse_gzip_header(&buffer) {
        Ok((_, header)) => println!("{:#?}", header),
        Err(err) => eprintln!("Failed to parse GZIP header: {:?}", err),
    }
}