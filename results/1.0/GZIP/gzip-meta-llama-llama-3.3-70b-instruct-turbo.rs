use nom::{
    bytes::complete::{take, take_while},
    combinator::{map, opt},
    error::{context, ErrorKind, ParseError},
    number::complete::{be_u16, be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::{env, fs};

#[derive(Debug, PartialEq)]
enum GzipOs {
    FAT,
    Amiga,
    VMS,
    Unix,
    VM,
    Atari,
    HPFS,
    Macintosh,
    ZSystem,
    CPm,
    Tops20,
    NTFS,
    QDOS,
    AcornRISCOS,
    Unknown(u8),
}

impl GzipOs {
    fn from_u8(n: u8) -> Self {
        match n {
            0 => GzipOs::FAT,
            1 => GzipOs::Amiga,
            2 => GzipOs::VMS,
            3 => GzipOs::Unix,
            4 => GzipOs::VM,
            5 => GzipOs::Atari,
            6 => GzipOs::HPFS,
            7 => GzipOs::Macintosh,
            8 => GzipOs::ZSystem,
            9 => GzipOs::CPm,
            10 => GzipOs::Tops20,
            11 => GzipOs::NTFS,
            12 => GzipOs::QDOS,
            13 => GzipOs::AcornRISCOS,
            _ => GzipOs::Unknown(n),
        }
    }
}

#[derive(Debug)]
struct GzipFile {
    id1: u8,
    id2: u8,
    cm: u8,
    flg: u8,
    mtime: u32,
    xfl: u8,
    os: GzipOs,
    xlen: u16,
    extra: Vec<u8>,
    fname: Option<Vec<u8>>,
    fcomment: Option<Vec<u8>>,
    hcrc: Option<u16>,
    compressed: Vec<u8>,
    isize: u32,
}

fn parse_gzip_data(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, data) = take_while(|c| *c != 0)(input)?;
    Ok((input, data.to_vec()))
}

fn parse_gzip(input: &[u8]) -> IResult<&[u8], GzipFile> {
    context(
        "Gzip",
        tuple((
            map(be_u8, |x: u8| {
                if x == 0x1f {
                    Ok(x)
                } else {
                    Err(nom::Err::Error(ErrorKind::AlphaNumeric))
                }
            }),
            map(be_u8, |x: u8| {
                if x == 0x8b {
                    Ok(x)
                } else {
                    Err(nom::Err::Error(ErrorKind::AlphaNumeric))
                }
            }),
            be_u8,
            be_u8,
            be_u32,
            be_u8,
            map(be_u8, GzipOs::from_u8),
            be_u16,
            take(0),
            opt(parse_gzip_data),
            opt(parse_gzip_data),
            opt(be_u16),
            take_while(|c| *c != 0),
            be_u32,
        )),
    )(input)
    .and_then(|(input, (
        id1,
        id2,
        cm,
        flg,
        mtime,
        xfl,
        os,
        xlen,
        extra,
        fname,
        fcomment,
        hcrc,
        compressed,
        isize,
    ))| {
        let extra_field = if xlen > 0 { Some(extra) } else { None };
        let fname = if fname.is_some() && fname.as_ref().unwrap().len() > 0 {
            Some(fname.unwrap())
        } else {
            None
        };
        let fcomment = if fcomment.is_some() && fcomment.as_ref().unwrap().len() > 0 {
            Some(fcomment.unwrap())
        } else {
            None
        };
        Ok((
            input,
            GzipFile {
                id1: id1,
                id2: id2,
                cm,
                flg,
                mtime,
                xfl,
                os,
                xlen,
                extra: extra_field.unwrap_or_default(),
                fname,
                fcomment,
                hcrc,
                compressed: compressed.to_vec(),
                isize,
            },
        ))
    })
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <gzip file>", args[0]);
        return;
    }
    let path = env::args().nth(1).unwrap();
    let file = fs::read(path).expect("Failed to read file");
    let result = parse_gzip(&file);
    match result {
        Ok((_, gzip_file)) => {
            println!("Gzip File: {:?}", gzip_file);
        }
        Err(err) => {
            println!("Error: {:?}", err);
        }
    }
}