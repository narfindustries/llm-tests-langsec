use nom::{
    bits::complete::{tag, take},
    bytes::complete::{take as take_bytes},
    combinator::{map, verify},
    error::Error,
    multi::many0,
    sequence::tuple,
    IResult,
};
use std::{env, fs, io::Read};

#[derive(Debug)]
struct GzipHeader {
    id1: u8,
    id2: u8,
    compression_method: u8,
    flags: GzipFlags,
    mtime: u32,
    extra_flags: u8,
    os: u8,
    extra_fields: Option<Vec<ExtraField>>,
    filename: Option<String>,
    comment: Option<String>,
    crc16: Option<u16>,
}

#[derive(Debug)]
struct GzipFlags {
    ftext: bool,
    fhcrc: bool,
    fextra: bool,
    fname: bool,
    fcomment: bool,
    reserved: u8,
}

#[derive(Debug)]
struct ExtraField {
    si1: u8,
    si2: u8,
    len: u16,
    data: Vec<u8>,
}

fn parse_gzip_flags(input: (&[u8], usize)) -> IResult<(&[u8], usize), GzipFlags> {
    map(
        tuple((
            take(1usize),
            take(1usize),
            take(1usize),
            take(1usize),
            take(1usize),
            take(3usize),
        )),
        |(ftext, fhcrc, fextra, fname, fcomment, reserved)| GzipFlags {
            ftext,
            fhcrc,
            fextra,
            fname,
            fcomment,
            reserved,
        },
    )(input)
}

fn parse_extra_field(input: &[u8]) -> IResult<&[u8], ExtraField> {
    let (input, si1) = take_bytes(1u8)(input)?;
    let (input, si2) = take_bytes(1u8)(input)?;
    let (input, len) = take_bytes(2u8)(input)?;
    let len = u16::from_le_bytes([len[0], len[1]]);
    let (input, data) = take_bytes(len)(input)?;
    
    Ok((input, ExtraField {
        si1: si1[0],
        si2: si2[0],
        len,
        data: data.to_vec(),
    }))
}

fn parse_null_terminated_string(input: &[u8]) -> IResult<&[u8], String> {
    let mut end = 0;
    while end < input.len() && input[end] != 0 {
        end += 1;
    }
    if end == input.len() {
        return Err(nom::Err::Error(Error::new(input, nom::error::ErrorKind::Tag)));
    }
    let (remaining, string_bytes) = take_bytes(end)(input)?;
    let (remaining, _) = take_bytes(1u8)(remaining)?; // consume null terminator
    Ok((remaining, String::from_utf8_lossy(string_bytes).into_owned()))
}

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    let (input, id1) = verify(take_bytes(1u8), |b: &[u8]| b[0] == 0x1f)(input)?;
    let (input, id2) = verify(take_bytes(1u8), |b: &[u8]| b[0] == 0x8b)(input)?;
    let (input, compression_method) = verify(take_bytes(1u8), |b: &[u8]| b[0] == 8)(input)?;
    
    let (input, flags_byte) = take_bytes(1u8)(input)?;
    let ((_, _), flags) = parse_gzip_flags((&[flags_byte[0]], 0))?;
    
    let (input, mtime_bytes) = take_bytes(4u8)(input)?;
    let mtime = u32::from_le_bytes([mtime_bytes[0], mtime_bytes[1], mtime_bytes[2], mtime_bytes[3]]);
    
    let (input, extra_flags) = take_bytes(1u8)(input)?;
    let (input, os) = take_bytes(1u8)(input)?;
    
    let mut current_input = input;
    
    let extra_fields = if flags.fextra {
        let (input, xlen_bytes) = take_bytes(2u8)(current_input)?;
        let xlen = u16::from_le_bytes([xlen_bytes[0], xlen_bytes[1]]);
        let (input, extra_data) = take_bytes(xlen)(input)?;
        let mut extra_fields = Vec::new();
        let mut extra_input = extra_data;
        while !extra_input.is_empty() {
            let (remaining, field) = parse_extra_field(extra_input)?;
            extra_fields.push(field);
            extra_input = remaining;
        }
        current_input = input;
        Some(extra_fields)
    } else {
        None
    };
    
    let (input, filename) = if flags.fname {
        let (input, name) = parse_null_terminated_string(current_input)?;
        current_input = input;
        Some(name)
    } else {
        current_input = current_input;
        None
    };
    
    let (input, comment) = if flags.fcomment {
        let (input, comment) = parse_null_terminated_string(current_input)?;
        current_input = input;
        Some(comment)
    } else {
        current_input = current_input;
        None
    };
    
    let (input, crc16) = if flags.fhcrc {
        let (input, crc_bytes) = take_bytes(2u8)(current_input)?;
        current_input = input;
        Some(u16::from_le_bytes([crc_bytes[0], crc_bytes[1]]))
    } else {
        current_input = current_input;
        None
    };
    
    Ok((current_input, GzipHeader {
        id1: id1[0],
        id2: id2[0],
        compression_method: compression_method[0],
        flags,
        mtime,
        extra_flags: extra_flags[0],
        os: os[0],
        extra_fields,
        filename,
        comment,
        crc16,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <gzip-file>", args[0]);
        std::process::exit(1);
    }

    let mut file = fs::File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_gzip_header(&buffer) {
        Ok((_, header)) => println!("{:#?}", header),
        Err(e) => eprintln!("Failed to parse GZIP header: {:?}", e),
    }
}