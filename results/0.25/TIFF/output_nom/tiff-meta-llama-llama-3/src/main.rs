use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_opt, opt, verify},
    multi::{many0, many1},
    number::complete::{be_f32, be_f64, be_i16, be_i32, be_i8, be_u16, be_u32, be_u8},
    sequence::{pair, tuple},
    IResult,
};
use std::{
    env, fs,
    io::{self, Read},
    str,
};

#[derive(Debug, PartialEq)]
enum ByteOrder {
    LittleEndian,
    BigEndian,
}

impl ByteOrder {
    fn from_bytes(bytes: &[u8]) -> IResult<&[u8], ByteOrder> {
        if bytes.starts_with(b"II") {
            Ok((&bytes[2..], ByteOrder::LittleEndian))
        } else if bytes.starts_with(b"MM") {
            Ok((&bytes[2..], ByteOrder::BigEndian))
        } else {
            Err(nom::Err::Error(nom::error::ErrorKind::AlphaNumeric))
        }
    }
}

#[derive(Debug, PartialEq)]
enum FieldType {
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

impl FieldType {
    fn from_u16(value: u16) -> FieldType {
        match value {
            1 => FieldType::Byte,
            2 => FieldType::Ascii,
            3 => FieldType::Short,
            4 => FieldType::Long,
            5 => FieldType::Rational,
            6 => FieldType::Sbyte,
            7 => FieldType::Undefined,
            8 => FieldType::Sshort,
            9 => FieldType::Slong,
            10 => FieldType::Srational,
            11 => FieldType::Float,
            12 => FieldType::Double,
            _ => panic!("Invalid field type"),
        }
    }
}

#[derive(Debug, PartialEq)]
struct Field {
    tag: u16,
    field_type: FieldType,
    count: u32,
    value: Vec<u8>,
}

impl Field {
    fn parse(byte_order: ByteOrder, input: &[u8]) -> IResult<&[u8], Field> {
        let (input, tag) = be_u16(byte_order)(input)?;
        let (input, field_type) = map(be_u16, FieldType::from_u16)(input)?;
        let (input, count) = be_u32(byte_order)(input)?;
        let (input, value) = take(count as usize)(input)?;
        Ok((input, Field { tag, field_type, count, value: value.to_vec() }))
    }
}

#[derive(Debug, PartialEq)]
struct IFD {
    num_fields: u16,
    fields: Vec<Field>,
    next_ifd_offset: Option<u32>,
}

impl IFD {
    fn parse(byte_order: ByteOrder, input: &[u8]) -> IResult<&[u8], IFD> {
        let (input, num_fields) = be_u16(byte_order)(input)?;
        let (input, fields) = many1(|input| Field::parse(byte_order, input))(input)?;
        let (input, next_ifd_offset) = opt(be_u32(byte_order))(input)?;
        Ok((input, IFD { num_fields, fields, next_ifd_offset }))
    }
}

#[derive(Debug, PartialEq)]
struct TIFF {
    byte_order: ByteOrder,
    ifd: IFD,
}

impl TIFF {
    fn parse(input: &[u8]) -> IResult<&[u8], TIFF> {
        let (input, byte_order) = ByteOrder::from_bytes(input)?;
        let (input, _magic) = verify(take(2usize), |x: &[u8]| x == b"42")(input)?;
        let (input, ifd_offset) = be_u32(byte_order)(input)?;
        let (input, ifd) = IFD::parse(byte_order, &input[ifd_offset as usize..])?;
        Ok((input, TIFF { byte_order, ifd }))
    }
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <input_file>", args[0]);
    }

    let mut file = fs::File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match TIFF::parse(&buffer) {
        Ok((_, tiff)) => println!("{:?}", tiff),
        Err(err) => panic!("Error parsing TIFF: {:?}", err),
    }

    Ok(())
}

fn be_u8(byte_order: ByteOrder) -> impl Fn(&[u8]) -> IResult<&[u8], u8> {
    move |input| match byte_order {
        ByteOrder::LittleEndian => map(take(1usize), |x: &[u8]| x[0])(input),
        ByteOrder::BigEndian => map(take(1usize), |x: &[u8]| x[0])(input),
    }
}

fn be_u16(byte_order: ByteOrder) -> impl Fn(&[u8]) -> IResult<&[u8], u16> {
    move |input| match byte_order {
        ByteOrder::LittleEndian => map(take(2usize), |x: &[u8]| ((x[1] as u16) << 8) | (x[0] as u16))(input),
        ByteOrder::BigEndian => map(take(2usize), |x: &[u8]| ((x[0] as u16) << 8) | (x[1] as u16))(input),
    }
}

fn be_u32(byte_order: ByteOrder) -> impl Fn(&[u8]) -> IResult<&[u8], u32> {
    move |input| match byte_order {
        ByteOrder::LittleEndian => map(take(4usize), |x: &[u8]| ((x[3] as u32) << 24) | ((x[2] as u32) << 16) | ((x[1] as u32) << 8) | (x[0] as u32))(input),
        ByteOrder::BigEndian => map(take(4usize), |x: &[u8]| ((x[0] as u32) << 24) | ((x[1] as u32) << 16) | ((x[2] as u32) << 8) | (x[3] as u32))(input),
    }
}

fn be_i8(byte_order: ByteOrder) -> impl Fn(&[u8]) -> IResult<&[u8], i8> {
    be_u8(byte_order)
}

fn be_i16(byte_order: ByteOrder) -> impl Fn(&[u8]) -> IResult<&[u8], i16> {
    be_u16(byte_order)
}

fn be_i32(byte_order: ByteOrder) -> impl Fn(&[u8]) -> IResult<&[u8], i32> {
    be_u32(byte_order)
}

fn be_f32(byte_order: ByteOrder) -> impl Fn(&[u8]) -> IResult<&[u8], f32> {
    be_u32(byte_order)
}

fn be_f64(byte_order: ByteOrder) -> impl Fn(&[u8]) -> IResult<&[u8], f64> {
    map(take(8usize), |x: &[u8]| f64::from_be_bytes([x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7]]))(byte_order)
}