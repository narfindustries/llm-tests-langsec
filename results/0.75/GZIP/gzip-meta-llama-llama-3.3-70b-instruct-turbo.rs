use std::env;
use std::fs::File;
use std::io::Read;
use nom::{
    bytes::complete::{take, take_while_m_n},
    combinator::{map, verify},
    error::{Error, ErrorKind},
    number::complete::{be_u16, be_u32},
    IResult,
};

#[derive(Debug)]
enum Os {
    Fat,
    Amiga,
    Vms,
    Unix,
    Other(u8),
}

impl From<u8> for Os {
    fn from(value: u8) -> Self {
        match value {
            0x00 => Os::Fat,
            0x01 => Os::Amiga,
            0x02 => Os::Vms,
            0x03 => Os::Unix,
            _ => Os::Other(value),
        }
    }
}

#[derive(Debug)]
struct GzipHeader {
    id1: u8,
    id2: u8,
    cm: u8,
    flags: u8,
    mtime: u32,
    xfl: u8,
    os: Os,
}

#[derive(Debug)]
struct GzipExtra {
    len: u16,
    data: Vec<u8>,
}

#[derive(Debug)]
struct GzipFooter {
    crc32: u32,
    isize: u32,
}

fn parse_id1(input: &[u8]) -> IResult<&[u8], u8> {
    map(verify(take(1u8), |id1: &[u8]| id1[0] == 0x1F), |id1: &[u8]| id1[0])(input)
}

fn parse_id2(input: &[u8]) -> IResult<&[u8], u8> {
    map(verify(take(1u8), |id2: &[u8]| id2[0] == 0x8B), |id2: &[u8]| id2[0])(input)
}

fn parse_cm(input: &[u8]) -> IResult<&[u8], u8> {
    map(verify(take(1u8), |cm: &[u8]| cm[0] == 8), |cm: &[u8]| cm[0])(input)
}

fn parse_flags(input: &[u8]) -> IResult<&[u8], u8> {
    map(take(1u8), |flags: &[u8]| flags[0])(input)
}

fn parse_mtime(input: &[u8]) -> IResult<&[u8], u32> {
    be_u32(input)
}

fn parse_xfl(input: &[u8]) -> IResult<&[u8], u8> {
    map(take(1u8), |xfl: &[u8]| xfl[0])(input)
}

fn parse_os(input: &[u8]) -> IResult<&[u8], Os> {
    map(take(1u8), |os: &[u8]| Os::from(os[0]))(input)
}

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    let (input, id1) = parse_id1(input)?;
    let (input, id2) = parse_id2(input)?;
    let (input, cm) = parse_cm(input)?;
    let (input, flags) = parse_flags(input)?;
    let (input, mtime) = parse_mtime(input)?;
    let (input, xfl) = parse_xfl(input)?;
    let (input, os) = parse_os(input)?;
    Ok((input, GzipHeader { id1, id2, cm, flags, mtime, xfl, os }))
}

fn parse_gzip_extra(input: &[u8]) -> IResult<&[u8], GzipExtra> {
    let (input, len) = be_u16(input)?;
    let (input, data) = take(len)(input)?;
    Ok((input, GzipExtra { len, data: data.to_vec() }))
}

fn parse_gzip_footer(input: &[u8]) -> IResult<&[u8], GzipFooter> {
    let (input, crc32) = be_u32(input)?;
    let (input, isize) = be_u32(input)?;
    Ok((input, GzipFooter { crc32, isize }))
}

fn parse_gzip(input: &[u8]) -> IResult<&[u8], (GzipHeader, Option<GzipExtra>, Option<String>, Option<String>, GzipFooter)> {
    let (mut input, header) = parse_gzip_header(input)?;
    let mut extra = None;
    let mut fname = None;
    let mut fcomment = None;
    if header.flags & 0x04 != 0 {
        let (input_temp, extra_data) = parse_gzip_extra(input)?;
        input = input_temp;
        extra = Some(extra_data);
    }
    if header.flags & 0x08 != 0 {
        let (input_temp, fname_data) = take_while_m_n(1, 65535, |c| c != 0)(input)?;
        input = input_temp;
        fname = Some(String::from_utf8_lossy(fname_data).into_owned());
    }
    if header.flags & 0x10 != 0 {
        let (input_temp, fcomment_data) = take_while_m_n(1, 65535, |c| c != 0)(input)?;
        input = input_temp;
        fcomment = Some(String::from_utf8_lossy(fcomment_data).into_owned());
    }
    let (input, footer) = parse_gzip_footer(input)?;
    Ok((input, (header, extra, fname, fcomment, footer)))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <file>", args[0]);
        return;
    }
    let mut file = File::open(&args[1]).unwrap();
    let mut data = Vec::new();
    file.read_to_end(&mut data).unwrap();
    match parse_gzip(&data) {
        Ok((_, (header, extra, fname, fcomment, footer))) => {
            println!("Gzip Header:");
            println!("  ID1: 0x{:02x}", header.id1);
            println!("  ID2: 0x{:02x}", header.id2);
            println!("  CM: {}", header.cm);
            println!("  Flags: 0x{:02x}", header.flags);
            println!("  MTime: {}", header.mtime);
            println!("  XFL: 0x{:02x}", header.xfl);
            println!("  OS: {:?}", header.os);
            if let Some(extra_data) = extra {
                println!("Gzip Extra:");
                println!("  Len: {}", extra_data.len);
                println!("  Data: {:?}", extra_data.data);
            }
            if let Some(fname_data) = fname {
                println!("Gzip FNAME:");
                println!("  {}", fname_data);
            }
            if let Some(fcomment_data) = fcomment {
                println!("Gzip FCOMMENT:");
                println!("  {}", fcomment_data);
            }
            println!("Gzip Footer:");
            println!("  CRC32: 0x{:08x}", footer.crc32);
            println!("  ISize: {}", footer.isize);
        }
        Err(err) => {
            println!("Error parsing Gzip: {:?}", err);
        }
    }
}