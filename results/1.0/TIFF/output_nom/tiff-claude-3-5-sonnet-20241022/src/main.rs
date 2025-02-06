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
    byte_order: [u8; 2],
    version: u16,
    ifd_offset: u32,
}

#[derive(Debug)]
struct IfdEntry {
    tag: u16,
    field_type: u16,
    count: u32,
    value_offset: u32,
}

#[derive(Debug)]
struct Tiff {
    header: TiffHeader,
    ifd_entries: Vec<IfdEntry>,
}

fn parse_header(input: &[u8]) -> IResult<&[u8], TiffHeader> {
    let (input, byte_order) = take(2usize)(input)?;
    let mut byte_order_array = [0u8; 2];
    byte_order_array.copy_from_slice(byte_order);

    let (input, version, ifd_offset) = match byte_order {
        b"II" => {
            let (input, version) = le_u16(input)?;
            let (input, offset) = le_u32(input)?;
            (input, version, offset)
        },
        b"MM" => {
            let (input, version) = be_u16(input)?;
            let (input, offset) = be_u32(input)?;
            (input, version, offset)
        },
        _ => return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
    };

    Ok((input, TiffHeader {
        byte_order: byte_order_array,
        version,
        ifd_offset,
    }))
}

fn parse_ifd_entry(input: &[u8], is_little_endian: bool) -> IResult<&[u8], IfdEntry> {
    let (input, (tag, field_type, count, value_offset)) = if is_little_endian {
        tuple((le_u16, le_u16, le_u32, le_u32))(input)?
    } else {
        tuple((be_u16, be_u16, be_u32, be_u32))(input)?
    };

    Ok((input, IfdEntry {
        tag,
        field_type,
        count,
        value_offset,
    }))
}

fn parse_ifd(input: &[u8], is_little_endian: bool) -> IResult<&[u8], Vec<IfdEntry>> {
    let (input, entry_count) = if is_little_endian {
        le_u16(input)?
    } else {
        be_u16(input)?
    };

    let mut entries = Vec::with_capacity(entry_count as usize);
    let mut current_input = input;

    for _ in 0..entry_count {
        let (remaining, entry) = parse_ifd_entry(current_input, is_little_endian)?;
        entries.push(entry);
        current_input = remaining;
    }

    Ok((current_input, entries))
}

fn parse_tiff(input: &[u8]) -> IResult<&[u8], Tiff> {
    let (input, header) = parse_header(input)?;
    let is_little_endian = header.byte_order == *b"II";
    
    // Skip to IFD using the offset
    let ifd_start = input.get(header.ifd_offset as usize - 8..).ok_or_else(|| {
        nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Eof))
    })?;

    let (_, ifd_entries) = parse_ifd(ifd_start, is_little_endian)?;

    Ok((input, Tiff {
        header,
        ifd_entries,
    }))
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

#[derive(Debug, Clone, Copy)]
pub enum TagType {
    ImageWidth = 256,
    ImageLength = 257,
    BitsPerSample = 258,
    Compression = 259,
    PhotometricInterpretation = 262,
    StripOffsets = 273,
    SamplesPerPixel = 277,
    RowsPerStrip = 278,
    StripByteCounts = 279,
    XResolution = 282,
    YResolution = 283,
    ResolutionUnit = 296,
    SubfileType = 254,
    NewSubfileType = 255,
    Threshholding = 263,
    CellWidth = 264,
    CellLength = 265,
    FillOrder = 266,
    DocumentName = 269,
    ImageDescription = 270,
    Make = 271,
    Model = 272,
    MinSampleValue = 280,
    MaxSampleValue = 281,
    PlanarConfiguration = 284,
    PageName = 285,
    XPosition = 286,
    YPosition = 287,
    FreeOffsets = 288,
    FreeByteCounts = 289,
    GrayResponseUnit = 290,
    GrayResponseCurve = 291,
    ColorMap = 320,
    ExtraSamples = 338,
    Copyright = 33432,
}

#[derive(Debug, Clone, Copy)]
pub enum FieldType {
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

#[derive(Debug, Clone, Copy)]
pub enum Compression {
    None = 1,
    CCITT1D = 2,
    Group3Fax = 3,
    Group4Fax = 4,
    LZW = 5,
    JPEG = 6,
    PackBits = 32773,
}

#[derive(Debug, Clone, Copy)]
pub enum PhotometricInterpretation {
    WhiteIsZero = 0,
    BlackIsZero = 1,
    RGB = 2,
    Palette = 3,
    TransparencyMask = 4,
    CMYK = 5,
    YCbCr = 6,
}

#[derive(Debug, Clone, Copy)]
pub enum ResolutionUnit {
    None = 1,
    Inch = 2,
    Centimeter = 3,
}