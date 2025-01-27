use std::fs::File;
use std::io::Read;
use nom::{
    bytes::complete::{tag, take},
    combinator::{map, verify},
    multi::count,
    number::complete::{le_u16, le_u32, le_u8},
    sequence::tuple,
    IResult,
};

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
struct IFDEntry {
    tag: u16,
    field_type: u16,
    count: u32,
    value_offset: u32,
}

#[derive(Debug)]
struct IFD {
    entry_count: u16,
    entries: Vec<IFDEntry>,
    next_ifd_offset: u32,
}

fn parse_byte_order(input: &[u8]) -> IResult<&[u8], ByteOrder> {
    let (input, order_bytes) = take(2usize)(input)?;
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
    let (input, (byte_order, _, version, ifd_offset)) = tuple((
        parse_byte_order,
        verify(le_u16, |&x| x == 42),
        le_u16,
        le_u32,
    ))(input)?;

    Ok((
        input,
        TiffHeader {
            byte_order,
            version,
            ifd_offset,
        },
    ))
}

fn parse_ifd_entry(input: &[u8]) -> IResult<&[u8], IFDEntry> {
    let (input, (tag, field_type, count, value_offset)) =
        tuple((le_u16, le_u16, le_u32, le_u32))(input)?;

    Ok((
        input,
        IFDEntry {
            tag,
            field_type,
            count,
            value_offset,
        },
    ))
}

fn parse_ifd(input: &[u8]) -> IResult<&[u8], IFD> {
    let (input, entry_count) = le_u16(input)?;
    let (input, entries) = count(parse_ifd_entry, entry_count as usize)(input)?;
    let (input, next_ifd_offset) = le_u32(input)?;

    Ok((
        input,
        IFD {
            entry_count,
            entries,
            next_ifd_offset,
        },
    ))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <tiff_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_header(&buffer) {
        Ok((remaining, header)) => {
            println!("TIFF Header: {:?}", header);
            
            // Calculate the offset to the first IFD
            let ifd_start = &remaining[(header.ifd_offset as usize - 8)..];
            
            match parse_ifd(ifd_start) {
                Ok((_, ifd)) => {
                    println!("IFD: {:?}", ifd);
                }
                Err(e) => eprintln!("Failed to parse IFD: {:?}", e),
            }
        }
        Err(e) => eprintln!("Failed to parse TIFF header: {:?}", e),
    }
}