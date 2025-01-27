use std::env;
use std::fs::File;
use std::io::Read;
use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt, rest},
    number::complete::be_u16,
    sequence::{tuple, preceded},
    IResult,
};

#[derive(Debug)]
struct TiffHeader {
    byte_order: [u8; 2],
    version: u16,
    offset: u32,
}

#[derive(Debug)]
struct IfdEntry {
    tag: u16,
    field_type: u16,
    count: u32,
    value_offset: u32,
}

#[derive(Debug)]
enum TiffFieldType {
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


fn parse_tiff_header(input: &[u8]) -> IResult<&[u8], TiffHeader> {
    let (input, byte_order) = take(2usize)(input)?;
    let (input, version) = be_u16(input)?;
    let (input, offset) = preceded(take(2usize), be_u32)(input)?;
    Ok((input, TiffHeader { byte_order: byte_order.try_into().unwrap(), version, offset }))
}

fn parse_ifd_entry(input: &[u8]) -> IResult<&[u8], IfdEntry> {
    let (input, (tag, field_type, count, value_offset)) = tuple((be_u16, be_u16, be_u32, be_u32))(input)?;
    Ok((input, IfdEntry { tag, field_type, count, value_offset }))
}

fn parse_ifd(input: &[u8]) -> IResult<&[u8], Vec<IfdEntry>> {
    let (input, num_entries) = be_u16(input)?;
    let (input, entries) = nom::multi::count(parse_ifd_entry, num_entries as usize)(input)?;
    let (input, next_ifd_offset) = be_u32(input)?; //Next IFD offset
    Ok((input, entries))
}

fn be_u32(input: &[u8]) -> IResult<&[u8], u32> {
    map(take(4usize), |bytes: &[u8]| u32::from_be_bytes(bytes.try_into().unwrap()))(input)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <tiff_file>", args[0]);
        return Ok(());
    }

    let filename = &args[1];
    let mut file = File::open(filename)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_tiff_header(&buffer) {
        Ok((remaining, header)) => {
            println!("TIFF Header: {:?}", header);
            let ifd_offset = header.offset as usize;
            let ifd_data = &buffer[ifd_offset..];
            match parse_ifd(ifd_data) {
                Ok((_, ifd_entries)) => {
                    println!("IFD Entries: {:?}", ifd_entries);
                }
                Err(e) => {
                    println!("Error parsing IFD: {:?}", e);
                }
            }
        }
        Err(e) => {
            println!("Error parsing TIFF header: {:?}", e);
        }
    }

    Ok(())
}
