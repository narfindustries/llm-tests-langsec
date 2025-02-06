use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32, be_u16, be_u32},
    IResult,
    sequence::tuple,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct TiffHeader {
    byte_order: ByteOrder,
    version: u16,
    ifd_offset: u32,
}

#[derive(Debug)]
enum ByteOrder {
    LittleEndian,
    BigEndian,
}

#[derive(Debug)]
struct IfdEntry {
    tag: u16,
    field_type: FieldType,
    count: u32,
    value_offset: u32,
}

#[derive(Debug)]
enum FieldType {
    Byte,
    Ascii,
    Short,
    Long,
    Rational,
    SByte,
    Undefined,
    SShort,
    SLong,
    SRational,
    Float,
    Double,
}

impl FieldType {
    fn from_u16(value: u16) -> Option<FieldType> {
        match value {
            1 => Some(FieldType::Byte),
            2 => Some(FieldType::Ascii),
            3 => Some(FieldType::Short),
            4 => Some(FieldType::Long),
            5 => Some(FieldType::Rational),
            6 => Some(FieldType::SByte),
            7 => Some(FieldType::Undefined),
            8 => Some(FieldType::SShort),
            9 => Some(FieldType::SLong),
            10 => Some(FieldType::SRational),
            11 => Some(FieldType::Float),
            12 => Some(FieldType::Double),
            _ => None,
        }
    }
}

#[derive(Debug)]
struct Tiff {
    header: TiffHeader,
    ifds: Vec<Vec<IfdEntry>>,
}

fn parse_byte_order(input: &[u8]) -> IResult<&[u8], ByteOrder> {
    let (input, order_bytes) = take(2u8)(input)?;
    match order_bytes {
        b"II" => Ok((input, ByteOrder::LittleEndian)),
        b"MM" => Ok((input, ByteOrder::BigEndian)),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

fn parse_header(input: &[u8]) -> IResult<&[u8], TiffHeader> {
    let (input, (byte_order, version, ifd_offset)) = match parse_byte_order(input)? {
        (input, ByteOrder::LittleEndian) => {
            let (input, (_, version, ifd_offset)) = tuple((
                tag([0x2A, 0x00]),
                le_u16,
                le_u32,
            ))(input)?;
            (input, (ByteOrder::LittleEndian, version, ifd_offset))
        },
        (input, ByteOrder::BigEndian) => {
            let (input, (_, version, ifd_offset)) = tuple((
                tag([0x00, 0x2A]),
                be_u16,
                be_u32,
            ))(input)?;
            (input, (ByteOrder::BigEndian, version, ifd_offset))
        },
    };

    Ok((input, TiffHeader {
        byte_order,
        version,
        ifd_offset,
    }))
}

fn parse_ifd_entry<'a>(input: &'a [u8], byte_order: &ByteOrder) -> IResult<&'a [u8], IfdEntry> {
    let (input, (tag, field_type, count, value_offset)) = match byte_order {
        ByteOrder::LittleEndian => tuple((le_u16, le_u16, le_u32, le_u32))(input)?,
        ByteOrder::BigEndian => tuple((be_u16, be_u16, be_u32, be_u32))(input)?,
    };

    Ok((input, IfdEntry {
        tag,
        field_type: FieldType::from_u16(field_type).unwrap_or(FieldType::Undefined),
        count,
        value_offset,
    }))
}

fn parse_ifd<'a>(input: &'a [u8], byte_order: &ByteOrder) -> IResult<&'a [u8], Vec<IfdEntry>> {
    let (input, entry_count) = match byte_order {
        ByteOrder::LittleEndian => le_u16(input)?,
        ByteOrder::BigEndian => be_u16(input)?,
    };

    let mut entries = Vec::new();
    let mut remaining = input;

    for _ in 0..entry_count {
        let (new_remaining, entry) = parse_ifd_entry(remaining, byte_order)?;
        entries.push(entry);
        remaining = new_remaining;
    }

    Ok((remaining, entries))
}

fn parse_tiff(input: &[u8]) -> IResult<&[u8], Tiff> {
    let (mut input, header) = parse_header(input)?;
    let mut ifds = Vec::new();
    
    let mut next_ifd_offset = header.ifd_offset;
    while next_ifd_offset != 0 {
        input = &input[next_ifd_offset as usize - (input.len() - input.len())..];
        
        let (new_input, ifd) = parse_ifd(input, &header.byte_order)?;
        ifds.push(ifd);
        
        next_ifd_offset = match header.byte_order {
            ByteOrder::LittleEndian => {
                let (_, offset) = le_u32(new_input)?;
                offset
            },
            ByteOrder::BigEndian => {
                let (_, offset) = be_u32(new_input)?;
                offset
            },
        };
        
        input = new_input;
    }

    Ok((input, Tiff { header, ifds }))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <tiff_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_tiff(&buffer) {
        Ok((_, tiff)) => println!("{:#?}", tiff),
        Err(e) => eprintln!("Error parsing TIFF: {:?}", e),
    }

    Ok(())
}