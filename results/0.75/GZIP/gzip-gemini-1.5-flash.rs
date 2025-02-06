use nom::{
    bytes::complete::{tag, take, take_while_m_n},
    combinator::{map, map_res, opt},
    number::complete::{be_u16, be_u32, le_u16, le_u32},
    sequence::{tuple,},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

fn gzip_header(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = tag([0x1f, 0x8b])(input)?;
    let (input, cm) = be_u16(input)?;
    assert_eq!(cm, 8); // Check for deflate compression method
    let (input,flg) = be_u16(input)?;
    let (input, mtime) = be_u32(input)?;
    let (input, xfl) = be_u16(input)?;
    let (input, os) = be_u16(input)?;
    let ftext = (flg >> 0) & 1 != 0;
    let fhcrc = (flg >> 1) & 1 != 0;
    let fextra = (flg >> 2) & 1 != 0;
    let fname = (flg >> 3) & 1 != 0;
    let fcomment = (flg >> 4) & 1 != 0;

    let (input, extra_field) = opt(gzip_extra)(input)?;
    let (input, filename) = opt(gzip_filename)(input)?;
    let (input, comment) = opt(gzip_comment)(input)?;
    let (input, header_crc) = opt(le_u16)(input)?;

    Ok((input, ()))
}

fn gzip_extra(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, len) = be_u16(input)?;
    let (input, _) = take(len as usize)(input)?;
    Ok((input, ()))
}

fn gzip_filename(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, filename) = take_while_m_n(1, 1024, |b| b != 0)(input)?;
    let (input,_) = tag([0])(input)?;
    Ok((input, ()))
}

fn gzip_comment(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, comment) = take_while_m_n(1, 1024, |b| b != 0)(input)?;
    let (input,_) = tag([0])(input)?;
    Ok((input, ()))
}


fn gzip_footer(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, crc32) = be_u32(input)?;
    let (input, isize) = be_u32(input)?;
    Ok((input, ()))
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: gzip_parser <filename>");
        return;
    }
    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match gzip_header(&buffer) {
        Ok((remaining, _)) => {
            println!("Header parsed successfully!");
            match gzip_footer(remaining) {
                Ok((_,_)) => println!("Footer parsed successfully"),
                Err(e) => println!("Footer parsing failed: {:?}", e),
            }
        }
        Err(e) => println!("Header parsing failed: {:?}", e),
    }
}

