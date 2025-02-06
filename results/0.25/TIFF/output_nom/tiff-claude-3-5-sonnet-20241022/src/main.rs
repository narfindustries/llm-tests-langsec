use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32, be_u16, be_u32},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
enum ByteOrder {
    LittleEndian,
    BigEndian,
}

#[derive(Debug)]
struct TiffHeader {
    byte_order: ByteOrder,
    version: u16,
    ifd_offset: u32,
}

#[derive(Debug)]
enum DataType {
    Byte = 1,
    Ascii = 2,
    Short = 3,
    Long = 4,
    Rational = 5,
    SByte = 6,
    Undefined = 7,
    SShort = 8,
    SLong = 9,
    SRational = 10,
    Float = 11,
    Double = 12,
}

#[derive(Debug)]
struct IFDEntry {
    tag: u16,
    data_type: DataType,
    count: u32,
    value_offset: u32,
}

#[derive(Debug)]
struct TIFF {
    header: TiffHeader,
    ifd_entries: Vec<IFDEntry>,
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
            let (input, (_, version, ifd_offset)) = tuple((tag([0x2A, 0x00]), le_u16, le_u32))(input)?;
            (input, (ByteOrder::LittleEndian, version, ifd_offset))
        }
        (input, ByteOrder::BigEndian) => {
            let (input, (_, version, ifd_offset)) = tuple((tag([0x00, 0x2A]), be_u16, be_u32))(input)?;
            (input, (ByteOrder::BigEndian, version, ifd_offset))
        }
    };

    Ok((
        input,
        TiffHeader {
            byte_order,
            version,
            ifd_offset,
        },
    ))
}

fn parse_data_type(value: u16) -> DataType {
    match value {
        1 => DataType::Byte,
        2 => DataType::Ascii,
        3 => DataType::Short,
        4 => DataType::Long,
        5 => DataType::Rational,
        6 => DataType::SByte,
        7 => DataType::Undefined,
        8 => DataType::SShort,
        9 => DataType::SLong,
        10 => DataType::SRational,
        11 => DataType::Float,
        12 => DataType::Double,
        _ => DataType::Undefined,
    }
}

fn parse_ifd_entry<'a>(input: &'a [u8], byte_order: &ByteOrder) -> IResult<&'a [u8], IFDEntry> {
    let (input, (tag, type_val, count, value_offset)) = match byte_order {
        ByteOrder::LittleEndian => tuple((le_u16, le_u16, le_u32, le_u32))(input)?,
        ByteOrder::BigEndian => tuple((be_u16, be_u16, be_u32, be_u32))(input)?,
    };

    Ok((
        input,
        IFDEntry {
            tag,
            data_type: parse_data_type(type_val),
            count,
            value_offset,
        },
    ))
}

fn parse_ifd<'a>(input: &'a [u8], byte_order: &ByteOrder) -> IResult<&'a [u8], Vec<IFDEntry>> {
    let (mut input, entry_count) = match byte_order {
        ByteOrder::LittleEndian => le_u16(input)?,
        ByteOrder::BigEndian => be_u16(input)?,
    };

    let mut entries = Vec::new();
    for _ in 0..entry_count {
        let (new_input, entry) = parse_ifd_entry(input, byte_order)?;
        entries.push(entry);
        input = new_input;
    }

    Ok((input, entries))
}

fn parse_tiff(input: &[u8]) -> IResult<&[u8], TIFF> {
    let (input, header) = parse_header(input)?;
    let (input, ifd_entries) = parse_ifd(input, &header.byte_order)?;

    Ok((
        input,
        TIFF {
            header,
            ifd_entries,
        },
    ))
}

fn main() -> std::io::Result<()> {
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
        Err(e) => eprintln!("Failed to parse TIFF: {:?}", e),
    }

    Ok(())
}