use nom::{
    bytes::complete::{tag, take},
    combinator::{map},
    number::complete::{be_u16, be_u32},
    sequence::tuple,
    IResult,
    Err,
    error::ErrorKind,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
    clone::Clone,
};

#[derive(Debug)]
struct GzipFile {
    id1: u8,
    id2: u8,
    cm: u8,
    flags: u8,
    mtime: u32,
    xfl: u8,
    os: u8,
    extra: Option<Vec<u8>>,
    filename: Option<String>,
    comment: Option<String>,
    hcrc: Option<u16>,
    compressed_data: Vec<u8>,
    isize: u32,
}

fn parse_id1(input: &[u8]) -> IResult<&[u8], u8> {
    map(tag([0x1f]), |x: &[u8]| x[0])(input)
}

fn parse_id2(input: &[u8]) -> IResult<&[u8], u8> {
    map(tag([0x8b]), |x: &[u8]| x[0])(input)
}

fn parse_cm(input: &[u8]) -> IResult<&[u8], u8> {
    map(take(1u8), |x: &[u8]| x[0])(input)
}

fn parse_flags(input: &[u8]) -> IResult<&[u8], u8> {
    map(take(1u8), |x: &[u8]| x[0])(input)
}

fn parse_mtime(input: &[u8]) -> IResult<&[u8], u32> {
    be_u32(input)
}

fn parse_xfl(input: &[u8]) -> IResult<&[u8], u8> {
    map(take(1u8), |x: &[u8]| x[0])(input)
}

fn parse_os(input: &[u8]) -> IResult<&[u8], u8> {
    map(take(1u8), |x: &[u8]| x[0])(input)
}

fn parse_extra(input: &[u8]) -> IResult<&[u8], Option<Vec<u8>>> {
    let (input, flags) = parse_flags(input)?;
    if flags & 0x04 == 0 {
        Ok((input, None))
    } else {
        let (input, xlen) = be_u16(input)?;
        let (input, extra) = take(xlen)(input)?;
        Ok((input, Some(extra.to_vec())))
    }
}

fn parse_filename(input: &[u8]) -> IResult<&[u8], Option<String>> {
    let (input, flags) = parse_flags(input)?;
    if flags & 0x08 == 0 {
        Ok((input, None))
    } else {
        let (input, filename) = take_while(|x| x != 0)(input)?;
        let filename = String::from_utf8_lossy(filename).into_owned();
        Ok((input, Some(filename)))
    }
}

fn parse_comment(input: &[u8]) -> IResult<&[u8], Option<String>> {
    let (input, flags) = parse_flags(input)?;
    if flags & 0x10 == 0 {
        Ok((input, None))
    } else {
        let (input, comment) = take_while(|x| x != 0)(input)?;
        let comment = String::from_utf8_lossy(comment).into_owned();
        Ok((input, Some(comment)))
    }
}

fn parse_hcrc(input: &[u8]) -> IResult<&[u8], Option<u16>> {
    let (input, flags) = parse_flags(input)?;
    if flags & 0x02 == 0 {
        Ok((input, None))
    } else {
        be_u16(input).map(|(input, hcrc)| (input, Some(hcrc)))
    }
}

fn parse_gzip(input: &[u8]) -> IResult<&[u8], GzipFile> {
    let (input, (id1, id2, cm, flags, mtime, xfl, os)) = tuple((
        parse_id1,
        parse_id2,
        parse_cm,
        parse_flags,
        parse_mtime,
        parse_xfl,
        parse_os,
    ))(input)?;
    let (input, extra) = parse_extra(input)?;
    let (input, filename) = parse_filename(input)?;
    let (input, comment) = parse_comment(input)?;
    let (input, hcrc) = parse_hcrc(input)?;
    let (input, compressed_data) = take_while(|x| x != 0)(input)?;
    let (input, isize) = be_u32(input)?;
    Ok((
        input,
        GzipFile {
            id1,
            id2,
            cm,
            flags,
            mtime,
            xfl,
            os,
            extra,
            filename,
            comment,
            hcrc,
            compressed_data: compressed_data.to_vec(),
            isize,
        },
    ))
}

fn take_while<F>(f: F) -> impl Fn(&[u8]) -> IResult<&[u8], &[u8]>
where
    F: Fn(u8) -> bool + Clone,
{
    move |input: &[u8]| {
        let mut len = 0;
        let f_clone = f.clone();
        while len < input.len() && f_clone(input[len]) {
            len += 1;
        }
        if len == 0 {
            return Err(Err::Error((input, ErrorKind::AlphaNumeric)));
        }
        Ok((&input[len..], &input[..len]))
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return;
    }
    let file = File::open(&args[1]).unwrap();
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input).unwrap();
    match parse_gzip(&input) {
        Ok((_, gzip_file)) => println!("{:?}", gzip_file),
        Err(err) => println!("Error parsing GZIP file: {:?}", err),
    }
}