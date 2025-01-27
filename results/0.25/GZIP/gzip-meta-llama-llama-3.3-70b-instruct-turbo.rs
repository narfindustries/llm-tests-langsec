use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::take_while_m_n,
    number::complete::be_u16,
    sequence::{tuple, preceded},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{Read, BufReader};

#[derive(Debug)]
struct GzipHeader {
    id1: u8,
    id2: u8,
    compression_method: u8,
    flags: u8,
    modification_time: u32,
    extra_flags: u8,
    operating_system: u8,
}

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    map(
        tuple((
            tag([31, 139]),
            be_u8,
            be_u8,
            be_u8,
            be_u32,
            be_u8,
            be_u8,
        )),
        |(id1, id2, compression_method, flags, modification_time, extra_flags, operating_system)| {
            GzipHeader {
                id1,
                id2,
                compression_method,
                flags,
                modification_time,
                extra_flags,
                operating_system,
            }
        },
    )(input)
}

fn parse_optional_extra_field(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    preceded(
        be_u16,
        take_while_m_n(
            0,
            65535,
            |byte| byte != 0,
        ),
    )(input)
}

fn parse_subfield(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    preceded(be_u16, take_while_m_n(0, 65535, |byte| byte != 0))(input)
}

fn parse_filename(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    take_while_m_n(0, 65535, |byte| byte != 0)(input)
}

fn parse_gzip_footer(input: &[u8]) -> IResult<&[u8], (u32, u32)> {
    tuple((be_u32, be_u32))(input)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];
    let file = File::open(filename).unwrap();
    let mut reader = BufReader::new(file);
    let mut data = Vec::new();
    reader.read_to_end(&mut data).unwrap();

    let (input, header) = parse_gzip_header(&data).unwrap();
    println!("{:?}", header);

    let (input, extra_field) = opt(parse_optional_extra_field)(input).unwrap();
    println!("{:?}", extra_field);

    let (input, filename_field) = opt(parse_filename)(input).unwrap();
    println!("{:?}", filename_field);

    let (input, footer) = parse_gzip_footer(input).unwrap();
    println!("{:?}", footer);
}

fn be_u8(input: &[u8]) -> IResult<&[u8], u8> {
    map(take(1usize), |v: &[u8]| v[0])(input)
}

fn be_u32(input: &[u8]) -> IResult<&[u8], u32> {
    map(take(4usize), |v: &[u8]| ((v[0] as u32) << 24) | ((v[1] as u32) << 16) | ((v[2] as u32) << 8) | (v[3] as u32))(input)
}