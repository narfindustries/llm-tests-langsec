use nom::{
    bytes::complete::{tag, take, take_while},
    character::complete::{digit1, space0, space1},
    combinator::{map, opt},
    error::ParseError,
    multi::{many0, many1, count},
    sequence::{tuple, preceded, terminated},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct NitfFileHeader {
    fhdr: String,
    fver: String,
    clevel: u8,
    stype: char,
    encryp: u8,
    originr: String,
    oname: String,
    ophone: String,
    fl: u64,
    numi: u16,
    nums: u16,
    numx: u16,
    numd: u16,
    numr: u16,
}

#[derive(Debug)]
struct NitfImageSegment {
    im: String,
    isclas: char,
    encryp: u8,
    imag: String,
    tgtid: String,
    ititle: String,
    nsub: u8,
    nbands: u16,
    xbands: Option<u16>,
    imode: char,
    nbpp: u16,
    ic: String,
    comrat: String,
}

fn parse_fixed_string<'a, E: ParseError<&'a [u8]>>(length: usize) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], String, E> {
    move |input: &'a [u8]| {
        let (input, result) = take(length)(input)?;
        Ok((input, String::from_utf8_lossy(result).into_owned()))
    }
}

fn parse_nitf_file_header(input: &[u8]) -> IResult<&[u8], NitfFileHeader> {
    let (input, fhdr) = parse_fixed_string(4)(input)?;
    let (input, fver) = parse_fixed_string(5)(input)?;
    let (input, clevel) = map(take(1usize), |b: &[u8]| b[0] - b'0')(input)?;
    let (input, stype) = map(take(1usize), |b: &[u8]| b[0] as char)(input)?;
    let (input, encryp) = map(take(1usize), |b: &[u8]| b[0] - b'0')(input)?;
    let (input, originr) = parse_fixed_string(10)(input)?;
    let (input, oname) = parse_fixed_string(24)(input)?;
    let (input, ophone) = parse_fixed_string(18)(input)?;
    let (input, fl) = map(take(12usize), |b: &[u8]| String::from_utf8_lossy(b).parse().unwrap())(input)?;
    let (input, numi) = map(take(3usize), |b: &[u8]| String::from_utf8_lossy(b).parse().unwrap())(input)?;
    let (input, nums) = map(take(3usize), |b: &[u8]| String::from_utf8_lossy(b).parse().unwrap())(input)?;
    let (input, numx) = map(take(3usize), |b: &[u8]| String::from_utf8_lossy(b).parse().unwrap())(input)?;
    let (input, numd) = map(take(3usize), |b: &[u8]| String::from_utf8_lossy(b).parse().unwrap())(input)?;
    let (input, numr) = map(take(3usize), |b: &[u8]| String::from_utf8_lossy(b).parse().unwrap())(input)?;

    Ok((input, NitfFileHeader {
        fhdr, fver, clevel, stype, encryp, originr, 
        oname, ophone, fl, numi, nums, numx, numd, numr
    }))
}

fn parse_nitf_image_segment(input: &[u8]) -> IResult<&[u8], NitfImageSegment> {
    let (input, im) = parse_fixed_string(2)(input)?;
    let (input, isclas) = map(take(1usize), |b: &[u8]| b[0] as char)(input)?;
    let (input, encryp) = map(take(1usize), |b: &[u8]| b[0] - b'0')(input)?;
    let (input, imag) = parse_fixed_string(42)(input)?;
    let (input, tgtid) = parse_fixed_string(20)(input)?;
    let (input, ititle) = parse_fixed_string(80)(input)?;
    let (input, nsub) = map(take(1usize), |b: &[u8]| b[0] - b'0')(input)?;
    let (input, nbands) = map(take(3usize), |b: &[u8]| String::from_utf8_lossy(b).parse().unwrap())(input)?;
    let (input, xbands) = opt(map(take(5usize), |b: &[u8]| String::from_utf8_lossy(b).parse().unwrap()))(input)?;
    let (input, imode) = map(take(1usize), |b: &[u8]| b[0] as char)(input)?;
    let (input, nbpp) = map(take(2usize), |b: &[u8]| String::from_utf8_lossy(b).parse().unwrap())(input)?;
    let (input, ic) = parse_fixed_string(2)(input)?;
    let (input, comrat) = parse_fixed_string(4)(input)?;

    Ok((input, NitfImageSegment {
        im, isclas, encryp, imag, tgtid, ititle, 
        nsub, nbands, xbands, imode, nbpp, ic, comrat
    }))
}

fn parse_nitf(input: &[u8]) -> IResult<&[u8], (NitfFileHeader, Vec<NitfImageSegment>)> {
    let (input, file_header) = parse_nitf_file_header(input)?;
    let (input, image_segments) = count(parse_nitf_image_segment, file_header.numi as usize)(input)?;

    Ok((input, (file_header, image_segments)))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <nitf_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_nitf(&buffer) {
        Ok((_, (file_header, image_segments))) => {
            println!("File Header: {:?}", file_header);
            println!("Image Segments: {:?}", image_segments);
        },
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}