use nom::{
    bytes::complete::{tag, take},
    combinator::map,
    number::complete::{be_u16, be_u32, be_u8},
    IResult,
};
use std::{env, fs::File, io::Read};

#[derive(Debug)]
enum ByteOrder {
    Intel,
    Motorola,
}

#[derive(Debug)]
struct Ifh {
    byte_order: ByteOrder,
    version: u16,
    offset_to_ifd: u32,
}

fn parse_ifh(input: &[u8]) -> IResult<&[u8], Ifh> {
    let (input, byte_order) = map(be_u16, |x| match x {
        0x4949 => ByteOrder::Intel,
        0x4d4d => ByteOrder::Motorola,
        _ => panic!("Invalid byte order"),
    })(input)?;
    let (input, version) = be_u16(input)?;
    let (input, offset_to_ifd) = be_u32(input)?;
    Ok((input, Ifh { byte_order, version, offset_to_ifd }))
}

#[derive(Debug)]
enum DataType {
    Byte,
    Ascii,
    Short,
    Long,
    Rational,
}

#[derive(Debug)]
struct Tag {
    tag: u16,
    data_type: DataType,
    count: u32,
    value_offset: u32,
}

fn parse_tag(input: &[u8]) -> IResult<&[u8], Tag> {
    let (input, tag) = be_u16(input)?;
    let (input, data_type) = map(be_u16, |x| match x {
        1 => DataType::Byte,
        2 => DataType::Ascii,
        3 => DataType::Short,
        4 => DataType::Long,
        5 => DataType::Rational,
        _ => panic!("Invalid data type"),
    })(input)?;
    let (input, count) = be_u32(input)?;
    let (input, value_offset) = be_u32(input)?;
    Ok((input, Tag { tag, data_type, count, value_offset }))
}

#[derive(Debug)]
struct Ifd {
    number_of_directory_entries: u16,
    directory_entries: Vec<Tag>,
    next_ifd_offset: u32,
}

fn parse_ifd(input: &[u8]) -> IResult<&[u8], Ifd> {
    let (input, number_of_directory_entries) = be_u16(input)?;
    let mut directory_entries = vec![];
    for _ in 0..number_of_directory_entries {
        let (input, tag) = parse_tag(input)?;
        directory_entries.push(tag);
    }
    let (input, next_ifd_offset) = be_u32(input)?;
    Ok((input, Ifd {
        number_of_directory_entries,
        directory_entries,
        next_ifd_offset,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <input_file>", args[0]);
    }
    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut input = vec![];
    file.read_to_end(&mut input).expect("Failed to read file");
    let (input, ifh) = parse_ifh(&input).expect("Failed to parse IFH");
    let (input, ifd) = parse_ifd(&input[ifh.offset_to_ifd as usize..]).expect("Failed to parse IFD");
    println!("{:?}", ifh);
    println!("{:?}", ifd);
}