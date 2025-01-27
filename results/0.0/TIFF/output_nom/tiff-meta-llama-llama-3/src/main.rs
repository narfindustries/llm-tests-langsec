use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::{many0, many1},
    number::complete::{be_u16, be_u32, be_u8},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
};

#[derive(Debug)]
enum TiffByteOrder {
    LittleEndian,
    BigEndian,
}

#[derive(Debug)]
enum TiffFieldType {
    Byte,
    Ascii,
    Short,
    Long,
    Rational,
    Sbyte,
    Undefined,
    Sshort,
    Slong,
    Srational,
    Float,
    Double,
}

#[derive(Debug)]
struct TiffField {
    tag: u16,
    field_type: TiffFieldType,
    count: u32,
    value: Vec<u8>,
}

#[derive(Debug)]
struct TiffIfd {
    num_entries: u16,
    fields: Vec<TiffField>,
    next_ifd_offset: Option<u32>,
}

fn parse_tiff_byte_order(input: &[u8]) -> IResult<&[u8], TiffByteOrder> {
    alt((tag("II\0\0"), tag("MM\0\0")))(input).map(|(input, byte_order)| {
        let byte_order = match byte_order {
            b"II\0\0" => TiffByteOrder::LittleEndian,
            b"MM\0\0" => TiffByteOrder::BigEndian,
            _ => unreachable!(),
        };
        (input, byte_order)
    })
}

fn parse_tiff_field_type(input: &[u8], byte_order: TiffByteOrder) -> IResult<&[u8], TiffFieldType> {
    match byte_order {
        TiffByteOrder::LittleEndian => map(be_u16, |field_type| match field_type {
            1 => TiffFieldType::Byte,
            2 => TiffFieldType::Ascii,
            3 => TiffFieldType::Short,
            4 => TiffFieldType::Long,
            5 => TiffFieldType::Rational,
            6 => TiffFieldType::Sbyte,
            7 => TiffFieldType::Undefined,
            8 => TiffFieldType::Sshort,
            9 => TiffFieldType::Slong,
            10 => TiffFieldType::Srational,
            11 => TiffFieldType::Float,
            12 => TiffFieldType::Double,
            _ => panic!("Invalid field type"),
        })(input),
        TiffByteOrder::BigEndian => map(be_u16, |field_type| match field_type {
            1 => TiffFieldType::Byte,
            2 => TiffFieldType::Ascii,
            3 => TiffFieldType::Short,
            4 => TiffFieldType::Long,
            5 => TiffFieldType::Rational,
            6 => TiffFieldType::Sbyte,
            7 => TiffFieldType::Undefined,
            8 => TiffFieldType::Sshort,
            9 => TiffFieldType::Slong,
            10 => TiffFieldType::Srational,
            11 => TiffFieldType::Float,
            12 => TiffFieldType::Double,
            _ => panic!("Invalid field type"),
        })(input),
    }
}

fn parse_tiff_field(input: &[u8], byte_order: TiffByteOrder) -> IResult<&[u8], TiffField> {
    let (input, tag) = be_u16(input)?;
    let (input, field_type) = parse_tiff_field_type(input, byte_order)?;
    let (input, count) = be_u32(input)?;
    let (input, value) = take(4)(input)?;
    Ok((input, TiffField { tag, field_type, count, value: value.to_vec() }))
}

fn parse_tiff_ifd(input: &[u8], byte_order: TiffByteOrder) -> IResult<&[u8], TiffIfd> {
    let (input, num_entries) = be_u16(input)?;
    let (input, fields) = many1(|input| parse_tiff_field(input, byte_order))(input)?;
    let (input, next_ifd_offset) = map_opt(be_u32, |offset| if offset == 0 { None } else { Some(offset) })(input)?;
    Ok((input, TiffIfd { num_entries, fields, next_ifd_offset }))
}

fn parse_tiff(input: &[u8]) -> IResult<&[u8], (TiffByteOrder, TiffIfd)> {
    let (input, byte_order) = parse_tiff_byte_order(input)?;
    let (input, _) = be_u16(input)?; // magic number
    let (input, ifd_offset) = be_u32(input)?;
    let (input, ifd) = parse_tiff_ifd(&input[ifd_offset as usize..], byte_order)?;
    Ok((input, (byte_order, ifd)))
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
    let (_, (byte_order, ifd)) = parse_tiff(&input).unwrap();
    println!("Byte order: {:?}", byte_order);
    println!("IFD: {:?}", ifd);
}