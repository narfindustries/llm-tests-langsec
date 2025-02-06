use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32, be_u16, be_u32},
    IResult,
    sequence::tuple,
    multi::many0,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct TiffHeader {
    byte_order: ByteOrder,
    version: u16,
    first_ifd_offset: u32,
}

#[derive(Debug)]
enum ByteOrder {
    LittleEndian,
    BigEndian,
}

#[derive(Debug)]
struct IfdEntry {
    tag: u16,
    type_: u16,
    count: u32,
    value_offset: u32,
}

#[derive(Debug)]
struct Ifd {
    entry_count: u16,
    entries: Vec<IfdEntry>,
    next_ifd_offset: u32,
}

fn parse_byte_order(input: &[u8]) -> IResult<&[u8], ByteOrder> {
    let (input, bytes) = take(2usize)(input)?;
    match bytes {
        b"II" => Ok((input, ByteOrder::LittleEndian)),
        b"MM" => Ok((input, ByteOrder::BigEndian)),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

fn parse_header(input: &[u8]) -> IResult<&[u8], TiffHeader> {
    let (input, (byte_order, version, first_ifd_offset)) = tuple((
        parse_byte_order,
        le_u16,
        le_u32,
    ))(input)?;

    Ok((input, TiffHeader {
        byte_order,
        version,
        first_ifd_offset,
    }))
}

fn parse_ifd_entry(input: &[u8]) -> IResult<&[u8], IfdEntry> {
    let (input, (tag, type_, count, value_offset)) = tuple((
        le_u16,
        le_u16,
        le_u32,
        le_u32,
    ))(input)?;

    Ok((input, IfdEntry {
        tag,
        type_,
        count,
        value_offset,
    }))
}

fn parse_ifd(input: &[u8]) -> IResult<&[u8], Ifd> {
    let (input, entry_count) = le_u16(input)?;
    let (input, entries) = many0(parse_ifd_entry)(input)?;
    let (input, next_ifd_offset) = le_u32(input)?;

    Ok((input, Ifd {
        entry_count,
        entries,
        next_ifd_offset,
    }))
}

fn parse_tiff(input: &[u8]) -> IResult<&[u8], (TiffHeader, Vec<Ifd>)> {
    let (mut input, header) = parse_header(input)?;
    let mut ifds = Vec::new();
    let mut next_offset = header.first_ifd_offset;

    while next_offset != 0 {
        let offset = next_offset as usize;
        if offset >= input.len() {
            break;
        }
        let (_, ifd) = parse_ifd(&input[offset..])?;
        next_offset = ifd.next_ifd_offset;
        ifds.push(ifd);
    }

    Ok((input, (header, ifds)))
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
        Ok((_, (header, ifds))) => {
            println!("TIFF Header: {:?}", header);
            for (i, ifd) in ifds.iter().enumerate() {
                println!("IFD {}: {:?}", i, ifd);
            }
        }
        Err(e) => eprintln!("Error parsing TIFF: {:?}", e),
    }

    Ok(())
}

#[derive(Debug)]
enum TiffTag {
    ImageWidth(u32),
    ImageLength(u32),
    BitsPerSample(Vec<u16>),
    Compression(u16),
    PhotometricInterpretation(u16),
    StripOffsets(Vec<u32>),
    SamplesPerPixel(u16),
    RowsPerStrip(u32),
    StripByteCounts(Vec<u32>),
    XResolution(f64),
    YResolution(f64),
    ResolutionUnit(u16),
    ColorMap(Vec<u16>),
    PlanarConfiguration(u16),
    Software(String),
    DateTime(String),
    Artist(String),
    Copyright(String),
}

impl TiffTag {
    fn from_ifd_entry(entry: &IfdEntry, data: &[u8]) -> Option<TiffTag> {
        match entry.tag {
            256 => Some(TiffTag::ImageWidth(entry.value_offset)),
            257 => Some(TiffTag::ImageLength(entry.value_offset)),
            258 => None, // BitsPerSample requires parsing value array
            259 => Some(TiffTag::Compression(entry.value_offset as u16)),
            262 => Some(TiffTag::PhotometricInterpretation(entry.value_offset as u16)),
            277 => Some(TiffTag::SamplesPerPixel(entry.value_offset as u16)),
            284 => Some(TiffTag::PlanarConfiguration(entry.value_offset as u16)),
            296 => Some(TiffTag::ResolutionUnit(entry.value_offset as u16)),
            _ => None, // Other tags require more complex parsing
        }
    }
}