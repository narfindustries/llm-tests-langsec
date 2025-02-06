use nom::{
    bytes::complete::{take},
    combinator::{map},
    multi::{many1},
    number::complete::{be_u16, be_u32},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
};

#[derive(Debug, Clone)]
enum TagType {
    Byte,
    Ascii,
    Short,
    Long,
    Rational,
    SByte,
    Undefined,
    SShort,
    SLong,
    Float,
    Double,
}

#[derive(Debug, Clone)]
struct Tag {
    number: u16,
    tag_type: TagType,
    count: u32,
    value_offset: u32,
}

#[derive(Debug, Clone)]
struct Ifd {
    num_entries: u16,
    tags: Vec<Tag>,
    next_ifd_offset: u32,
}

#[derive(Debug, Clone)]
struct Tiff {
    byte_order: [u8; 2],
    magic_number: u16,
    ifd_offset: u32,
    ifds: Vec<Ifd>,
}

fn parse_byte_order(input: &[u8]) -> IResult<&[u8], [u8; 2]> {
    map(take(2usize), |x: &[u8]| {
        let mut arr = [0u8; 2];
        arr.copy_from_slice(x);
        arr
    })(input)
}

fn parse_magic_number(input: &[u8]) -> IResult<&[u8], u16> {
    map(be_u16, |n| n)(input)
}

fn parse_ifd_offset(input: &[u8]) -> IResult<&[u8], u32> {
    map(be_u32, |n| n)(input)
}

fn parse_tag_type(input: &[u8]) -> IResult<&[u8], TagType> {
    map(be_u16, |n| match n {
        1 => TagType::Byte,
        2 => TagType::Ascii,
        3 => TagType::Short,
        4 => TagType::Long,
        5 => TagType::Rational,
        6 => TagType::SByte,
        7 => TagType::Undefined,
        8 => TagType::SShort,
        9 => TagType::SLong,
        10 => TagType::Float,
        11 => TagType::Double,
        _ => panic!("Invalid tag type"),
    })(input)
}

fn parse_tag(input: &[u8]) -> IResult<&[u8], Tag> {
    let (input, number) = be_u16(input)?;
    let (input, tag_type) = parse_tag_type(input)?;
    let (input, count) = be_u32(input)?;
    let (input, value_offset) = be_u32(input)?;
    Ok((input, Tag { number, tag_type, count, value_offset }))
}

fn parse_ifd(input: &[u8]) -> IResult<&[u8], Ifd> {
    let (input, num_entries) = be_u16(input)?;
    let (input, tags) = many1(parse_tag)(input)?;
    let (input, next_ifd_offset) = be_u32(input)?;
    Ok((input, Ifd { num_entries, tags, next_ifd_offset }))
}

fn parse_tiff(input: &[u8]) -> IResult<&[u8], Tiff> {
    let (input, byte_order) = parse_byte_order(input)?;
    let (input, magic_number) = parse_magic_number(input)?;
    let (input, ifd_offset) = parse_ifd_offset(input)?;
    let mut ifds = Vec::new();
    let mut current_ifd_offset = ifd_offset;
    while current_ifd_offset != 0 {
        let ifd_input = &input[current_ifd_offset as usize..];
        let (_ifd_input, ifd) = parse_ifd(ifd_input)?;
        ifds.push(ifd.clone());
        current_ifd_offset = ifd.next_ifd_offset;
    }
    Ok((input, Tiff { byte_order, magic_number, ifd_offset, ifds }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <input_file>", args[0]);
    }
    let file = File::open(&args[1]).unwrap();
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input).unwrap();
    let (_input, tiff) = parse_tiff(&input).unwrap();
    println!("{:?}", tiff);
}