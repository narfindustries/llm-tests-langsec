use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::{many1},
    number::complete::{be_u16, be_u32},
    IResult,
};
use std::{env, fs};

#[derive(Debug)]
enum ByteOrder {
    LittleEndian,
    BigEndian,
}

#[derive(Debug)]
struct TiffHeader {
    byte_order: ByteOrder,
    version: u16,
    offset_to_first_ifd: u32,
}

fn parse_tiff_header(input: &[u8]) -> IResult<&[u8], TiffHeader> {
    let (input, _) = tag("\x49\x49")(input)?;
    let byte_order = ByteOrder::LittleEndian;
    let (input, version) = be_u16(input)?;
    let (input, offset_to_first_ifd) = be_u32(input)?;
    Ok((input, TiffHeader { byte_order, version, offset_to_first_ifd }))
}

#[derive(Debug)]
enum FieldType {
    Byte,
    Ascii,
    Short,
    Long,
    Rational,
}

#[derive(Debug)]
struct Field {
    tag: u16,
    field_type: FieldType,
    count: u32,
    value: Vec<u8>,
}

fn parse_field(input: &[u8]) -> IResult<&[u8], Field> {
    let (input, tag) = be_u16(input)?;
    let (input, field_type) = be_u16(input)?;
    let field_type = match field_type {
        1 => FieldType::Byte,
        2 => FieldType::Ascii,
        3 => FieldType::Short,
        4 => FieldType::Long,
        5 => FieldType::Rational,
        _ => panic!("Invalid field type"),
    };
    let (input, count) = be_u32(input)?;
    let (input, value) = take(4u8)(input)?;
    Ok((input, Field { tag, field_type, count, value: value.to_vec() }))
}

#[derive(Debug)]
struct Ifd {
    num_entries: u16,
    entries: Vec<Field>,
    offset_to_next_ifd: u32,
}

fn parse_ifd(input: &[u8]) -> IResult<&[u8], Ifd> {
    let (input, num_entries) = be_u16(input)?;
    let (input, entries) = many1(parse_field)(input)?;
    let (input, offset_to_next_ifd) = be_u32(input)?;
    Ok((input, Ifd { num_entries, entries, offset_to_next_ifd }))
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        return;
    }

    let input_file = &args[1];

    let data = fs::read(input_file).expect("Failed to read input file");

    let (_input, tiff_header) = parse_tiff_header(&data).expect("Failed to parse TIFF header");

    println!("TIFF Header: {:?}", tiff_header);

    let mut offset = tiff_header.offset_to_first_ifd as usize;

    while offset < data.len() {
        let (_input, ifd) = parse_ifd(&data[offset..]).expect("Failed to parse IFD");

        println!("IFD: {:?}", ifd);

        offset += ifd.offset_to_next_ifd as usize;
    }
}