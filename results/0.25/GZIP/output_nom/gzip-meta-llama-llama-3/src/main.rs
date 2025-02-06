use nom::{
    bytes::complete::{tag, take, take_while},
    combinator::{map},
    number::complete::{be_u16, be_u32, be_u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{Read, Result};
use std::path::Path;

fn gzip_id1(input: &[u8]) -> IResult<&[u8], u8> {
    map(tag([0x1f]), |x: &[u8]| x[0])(input)
}

fn gzip_id2(input: &[u8]) -> IResult<&[u8], u8> {
    map(tag([0x8b]), |x: &[u8]| x[0])(input)
}

fn gzip_cm(input: &[u8]) -> IResult<&[u8], u8> {
    map(be_u8, |x: u8| x)(input)
}

fn gzip_flags(input: &[u8]) -> IResult<&[u8], u8> {
    map(be_u8, |x: u8| x)(input)
}

fn gzip_mtime(input: &[u8]) -> IResult<&[u8], u32> {
    map(be_u32, |x: u32| x)(input)
}

fn gzip_xfl(input: &[u8]) -> IResult<&[u8], u8> {
    map(be_u8, |x: u8| x)(input)
}

fn gzip_os(input: &[u8]) -> IResult<&[u8], u8> {
    map(be_u8, |x: u8| x)(input)
}

fn gzip_xlen(input: &[u8]) -> IResult<&[u8], u16> {
    map(be_u16, |x: u16| x)(input)
}

fn gzip_extra(input: &[u8], len: u16) -> IResult<&[u8], &[u8]> {
    take(len as usize)(input)
}

fn gzip_fname(input: &[u8]) -> IResult<&[u8], &[u8]> {
    take_while(|x| x != 0)(input)
}

fn gzip_fcomment(input: &[u8]) -> IResult<&[u8], &[u8]> {
    take_while(|x| x != 0)(input)
}

fn gzip_hcrc(input: &[u8]) -> IResult<&[u8], u16> {
    map(be_u16, |x: u16| x)(input)
}

fn gzip_comprlen(input: &[u8]) -> IResult<&[u8], u32> {
    map(be_u32, |x: u32| x)(input)
}

fn gzip_comprdata(input: &[u8], len: u32) -> IResult<&[u8], &[u8]> {
    take(len as usize)(input)
}

fn gzip_isize(input: &[u8]) -> IResult<&[u8], u32> {
    map(be_u32, |x: u32| x)(input)
}

fn gzip_crc32(input: &[u8]) -> IResult<&[u8], u32> {
    map(be_u32, |x: u32| x)(input)
}

fn gzip(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = gzip_id1(input)?;
    let (input, _) = gzip_id2(input)?;
    let (input, _) = gzip_cm(input)?;
    let (input, flags) = gzip_flags(input)?;
    let (input, _) = gzip_mtime(input)?;
    let (input, _) = gzip_xfl(input)?;
    let (input, _) = gzip_os(input)?;
    if flags & 0x04 != 0 {
        let (input, len) = gzip_xlen(input)?;
        let (_input, _) = gzip_extra(input, len)?;
    }
    if flags & 0x08 != 0 {
        let (_input, _) = gzip_fname(input)?;
    }
    if flags & 0x10 != 0 {
        let (_input, _) = gzip_fcomment(input)?;
    }
    if flags & 0x02 != 0 {
        let (_input, _) = gzip_hcrc(input)?;
    }
    let (input, len) = gzip_comprlen(input)?;
    let (input, _) = gzip_comprdata(input, len)?;
    let (input, _) = gzip_isize(input)?;
    let (input, _) = gzip_crc32(input)?;
    Ok((input, ()))
}

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return Ok(());
    }
    let path = Path::new(&args[1]);
    let mut file = File::open(path)?;
    let mut data = Vec::new();
    file.read_to_end(&mut data)?;
    match gzip(&data) {
        Ok(_) => println!("GZIP file parsed successfully"),
        Err(err) => println!("Error parsing GZIP file: {:?}", err),
    }
    Ok(())
}