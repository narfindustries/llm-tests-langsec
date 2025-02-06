use nom::{
    bytes::complete::{tag, take},
    error::{ErrorKind, Err, ParseError},
    number::complete::{be_u16, be_u32, be_u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{Read, Result};

#[derive(Debug)]
struct GzipHeader {
    id1: u8,
    id2: u8,
    cm: u8,
    flags: u8,
    mtime: u32,
    xfl: u8,
    os: u8,
    extra: Option<Vec<u8>>,
    fname: Option<String>,
    fcomment: Option<String>,
    hcrc: Option<u16>,
}

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    let (input, _) = tag([0x1f, 0x8b])(input)?;
    let (input, cm) = be_u8(input)?;
    let (input, flags) = be_u8(input)?;
    let (input, mtime) = be_u32(input)?;
    let (input, xfl) = be_u8(input)?;
    let (input, os) = be_u8(input)?;

    let (input, extra) = if flags & 0x04 != 0 {
        let (input, xlen) = be_u16(input)?;
        let (input, extra) = take(xlen)(input)?;
        (input, Some(extra.to_vec()))
    } else {
        (input, None)
    };

    let (input, fname) = if flags & 0x08 != 0 {
        let (input, fname) = take_while(|c| c != 0)(input)?;
        (input, Some(String::from_utf8_lossy(fname).into_owned()))
    } else {
        (input, None)
    };

    let (input, fcomment) = if flags & 0x10 != 0 {
        let (input, fcomment) = take_while(|c| c != 0)(input)?;
        (input, Some(String::from_utf8_lossy(fcomment).into_owned()))
    } else {
        (input, None)
    };

    let (input, hcrc) = if flags & 0x02 != 0 {
        let (input, hcrc) = be_u16(input)?;
        (input, Some(hcrc))
    } else {
        (input, None)
    };

    Ok((
        input,
        GzipHeader {
            id1: 0x1f,
            id2: 0x8b,
            cm,
            flags,
            mtime,
            xfl,
            os,
            extra,
            fname,
            fcomment,
            hcrc,
        },
    ))
}

fn take_while<F>(predicate: F) -> impl Fn(&[u8]) -> IResult<&[u8], &[u8]>
where
    F: Fn(u8) -> bool + Clone,
{
    move |input: &[u8]| {
        let mut i = 0;
        while i < input.len() && predicate(input[i]) {
            i += 1;
        }
        if i == 0 {
            return Err(Err::Error(nom::error::Error::new(input, ErrorKind::NonEmpty)));
        }
        Ok((&input[i..], &input[..i]))
    }
}

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut data = Vec::new();
    file.read_to_end(&mut data)?;

    match parse_gzip_header(&data) {
        Ok((remaining, header)) => {
            println!("Gzip Header: {:?}", header);
            println!("Remaining data: {:?}", remaining);
        }
        Err(err) => {
            println!("Error parsing gzip header: {:?}", err);
        }
    }

    Ok(())
}