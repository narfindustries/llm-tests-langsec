use nom::{
    bytes::complete::{tag, take},
    combinator::map_res,
    number::complete::{le_u16, le_u32, le_u8},
    sequence::tuple,
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug)]
struct GzipHeader {
    id1: u8,
    id2: u8,
    compression_method: u8,
    flags: u8,
    mtime: u32,
    xfl: u8,
    os: u8,
    extra: Option<Vec<u8>>,
    fname: Option<String>,
    fcomment: Option<String>,
    fhcrc: Option<u16>,
}

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    let (input, (id1, id2, compression_method, flags, mtime, xfl, os)) =
        tuple((le_u8, le_u8, le_u8, le_u8, le_u32, le_u8, le_u8))(input)?;

    let (input, extra) = if flags & 0b0000_0100 != 0 {
        let (input, xlen) = le_u16(input)?;
        let (input, extra) = take(xlen)(input)?;
        (input, Some(extra.to_vec()))
    } else {
        (input, None)
    };

    let (input, fname) = if flags & 0b0000_1000 != 0 {
        let (input, fname) = take_until_null(input)?;
        (input, Some(fname))
    } else {
        (input, None)
    };

    let (input, fcomment) = if flags & 0b0001_0000 != 0 {
        let (input, fcomment) = take_until_null(input)?;
        (input, Some(fcomment))
    } else {
        (input, None)
    };

    let (input, fhcrc) = if flags & 0b0000_0010 != 0 {
        let (input, fhcrc) = le_u16(input)?;
        (input, Some(fhcrc))
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
            xfl,
            os,
            extra,
            fname,
            fcomment,
            fhcrc,
        },
    ))
}

fn take_until_null(input: &[u8]) -> IResult<&[u8], String> {
    map_res(take_while(|b| b != 0), |bytes: &[u8]| {
        String::from_utf8(bytes.to_vec())
    })(input)
}

fn take_while<F>(cond: F) -> impl Fn(&[u8]) -> IResult<&[u8], &[u8]>
where
    F: Fn(u8) -> bool,
{
    move |input: &[u8]| {
        let pos = input.iter().position(|&b| !cond(b)).unwrap_or(input.len());
        Ok((&input[pos..], &input[..pos]))
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_gzip_header(&buffer) {
        Ok((_, header)) => println!("{:?}", header),
        Err(e) => eprintln!("Failed to parse GZIP header: {:?}", e),
    }
}