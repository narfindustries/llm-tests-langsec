use std::env;
use std::fs::File;
use std::io::Read;
use nom::{
    bytes::complete::{tag, take},
    combinator::{map, verify},
    multi::take_till,
    number::complete::{be_u16, be_u32},
    IResult,
};

#[derive(Debug, PartialEq)]
enum Os {
    FAT,
    Amiga,
    VMS,
    Unix,
    Atari,
    OS2,
    Macintosh,
    TopSpeed,
    NTFS,
    QNX,
    Unknown(u8),
}

impl From<u8> for Os {
    fn from(os: u8) -> Self {
        match os {
            0 => Os::FAT,
            1 => Os::Amiga,
            2 => Os::VMS,
            3 => Os::Unix,
            4 => Os::Atari,
            5 => Os::OS2,
            6 => Os::Macintosh,
            7 => Os::TopSpeed,
            8 => Os::NTFS,
            9 => Os::QNX,
            _ => Os::Unknown(os),
        }
    }
}

fn compression_method(input: &[u8]) -> IResult<&[u8], u8> {
    let (input, method) = take(1u8)(input)?;
    verify(method, |m| *m == 8)?;
    Ok((input, method[0]))
}

fn flags(input: &[u8]) -> IResult<&[u8], u8> {
    take(1u8)(input)
}

fn extra_flags(input: &[u8]) -> IResult<&[u8], u8> {
    take(1u8)(input)
}

fn filename(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    take_till(|c| c == b'\x00')(input)
}

fn extra_field(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    take_till(|c| c == b'\x00')(input)
}

fn comment(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    take_till(|c| c == b'\x00')(input)
}

fn header_crc16(input: &[u8]) -> IResult<&[u8], u16> {
    be_u16(input)
}

fn gzip_header(input: &[u8]) -> IResult<&[u8], ([u8; 2], u8, u8, u8, Vec<u8>, Vec<u8>, Vec<u8>, u16)> {
    let (input, id1) = take(1u8)(input)?;
    let (input, id2) = take(1u8)(input)?;
    let (input, method) = compression_method(input)?;
    let (input, flags) = flags(input)?;
    let (input, time) = take(4u8)(input)?;
    let (input, extra_flags) = extra_flags(input)?;
    let (input, os) = take(1u8)(input)?;
    let (input, filename) = filename(input)?;
    let (input, extra_field) = extra_field(input)?;
    let (input, comment) = comment(input)?;
    let (input, header_crc16) = header_crc16(input)?;
    Ok((input, ([id1[0], id2[0]], method, flags, extra_flags, filename.to_vec(), extra_field.to_vec(), comment.to_vec(), header_crc16)))
}

fn gzip_footer(input: &[u8]) -> IResult<&[u8], (u32)> {
    let (input, crc32) = be_u32(input)?;
    let (input, isize) = be_u32(input)?;
    Ok((input, (crc32, isize)))
}

fn gzip(input: &[u8]) -> IResult<&[u8], (([u8; 2], u8, u8, u8, Vec<u8>, Vec<u8>, Vec<u8>, u16), (u32))> {
    let (input, header) = gzip_header(input)?;
    let (input, trailer) = gzip_footer(input)?;
    Ok((input, (header, trailer)))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let file = File::open(&args[1]).unwrap();
    let mut reader = std::io::BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input).unwrap();
    let result = gzip(&input);
    match result {
        Ok((remaining, (header, trailer))) => {
            println!("Header: {:?}", header);
            println!("Trailer: {:?}", trailer);
            if !remaining.is_empty() {
                println!("Warning: {} bytes remaining", remaining.len());
            }
        }
        Err(err) => {
            println!("Error: {:?}", err);
        }
    }
}