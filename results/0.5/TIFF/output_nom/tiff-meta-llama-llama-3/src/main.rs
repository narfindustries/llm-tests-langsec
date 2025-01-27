use nom::{
    bytes::complete::{tag, take},
    combinator::{map, verify},
    multi::{length_data, take_while_m_n},
    number::complete::{be_u16, be_u32, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{BufReader, Read};
use std::path::Path;

#[derive(Debug)]
enum TIFFByteOrder {
    LittleEndian,
    BigEndian,
}

#[derive(Debug)]
struct TIFFHeader {
    byte_order: TIFFByteOrder,
    tiff_magic: u16,
    offset_to_ifd: u32,
}

impl TIFFHeader {
    fn parse(input: &[u8]) -> IResult<&[u8], TIFFHeader> {
        let (input, byte_order) = verify(be_u16, |x| *x == 0x4949 || *x == 0x4d4d)(input)?;
        let byte_order = match byte_order {
            0x4949 => TIFFByteOrder::LittleEndian,
            0x4d4d => TIFFByteOrder::BigEndian,
            _ => unreachable!(),
        };
        let (input, tiff_magic) = be_u16(input)?;
        let (input, offset_to_ifd) = be_u32(input)?;
        Ok((input, TIFFHeader { byte_order, tiff_magic, offset_to_ifd }))
    }
}

#[derive(Debug)]
enum TIFFData {
    Byte(u8),
    Ascii(String),
    Short(u16),
    Long(u32),
    Rational(u32, u32),
    SByte(i8),
    Undefined(u8),
    SShort(i16),
    SLong(i32),
    SRational(i32, i32),
    Float(f32),
    Double(f64),
}

#[derive(Debug)]
struct TIFFIFDEntry {
    tag: u16,
    data_type: u16,
    count: u32,
    value: TIFFData,
}

impl TIFFIFDEntry {
    fn parse(input: &[u8], byte_order: TIFFByteOrder) -> IResult<&[u8], TIFFIFDEntry> {
        let (input, tag) = match byte_order {
            TIFFByteOrder::LittleEndian => le_u16(input)?,
            TIFFByteOrder::BigEndian => be_u16(input)?,
        };
        let (input, data_type) = match byte_order {
            TIFFByteOrder::LittleEndian => le_u16(input)?,
            TIFFByteOrder::BigEndian => be_u16(input)?,
        };
        let (input, count) = match byte_order {
            TIFFByteOrder::LittleEndian => le_u32(input)?,
            TIFFByteOrder::BigEndian => be_u32(input)?,
        };
        let (input, value) = match data_type {
            1 => {
                // BYTE
                let (input, value) = take(count as usize)(input)?;
                let mut value_iter = value.iter();
                let value = match value_iter.next() {
                    Some(&x) => TIFFData::Byte(x),
                    None => unreachable!(),
                };
                Ok((input, value))
            }
            2 => {
                // ASCII
                let (input, value) = take(count as usize)(input)?;
                let value = String::from_utf8_lossy(value).into_owned();
                Ok((input, TIFFData::Ascii(value)))
            }
            3 => {
                // SHORT
                let (input, value) = match byte_order {
                    TIFFByteOrder::LittleEndian => le_u16(input)?,
                    TIFFByteOrder::BigEndian => be_u16(input)?,
                };
                Ok((input, TIFFData::Short(value)))
            }
            4 => {
                // LONG
                let (input, value) = match byte_order {
                    TIFFByteOrder::LittleEndian => le_u32(input)?,
                    TIFFByteOrder::BigEndian => be_u32(input)?,
                };
                Ok((input, TIFFData::Long(value)))
            }
            5 => {
                // RATIONAL
                let (input, numerator) = match byte_order {
                    TIFFByteOrder::LittleEndian => le_u32(input)?,
                    TIFFByteOrder::BigEndian => be_u32(input)?,
                };
                let (input, denominator) = match byte_order {
                    TIFFByteOrder::LittleEndian => le_u32(input)?,
                    TIFFByteOrder::BigEndian => be_u32(input)?,
                };
                Ok((input, TIFFData::Rational(numerator, denominator)))
            }
            6 => {
                // SBYTE
                let (input, value) = take(1usize)(input)?;
                let mut value_iter = value.iter();
                let value = match value_iter.next() {
                    Some(&x) => TIFFData::SByte(x as i8),
                    None => unreachable!(),
                };
                Ok((input, value))
            }
            7 => {
                // UNDEFINED
                let (input, value) = take(1usize)(input)?;
                let mut value_iter = value.iter();
                let value = match value_iter.next() {
                    Some(&x) => TIFFData::Undefined(x),
                    None => unreachable!(),
                };
                Ok((input, value))
            }
            8 => {
                // SSHORT
                let (input, value) = match byte_order {
                    TIFFByteOrder::LittleEndian => le_i16(input)?,
                    TIFFByteOrder::BigEndian => be_i16(input)?,
                };
                Ok((input, TIFFData::SShort(value)))
            }
            9 => {
                // SLONG
                let (input, value) = match byte_order {
                    TIFFByteOrder::LittleEndian => le_i32(input)?,
                    TIFFByteOrder::BigEndian => be_i32(input)?,
                };
                Ok((input, TIFFData::SLong(value)))
            }
            10 => {
                // SRATIONAL
                let (input, numerator) = match byte_order {
                    TIFFByteOrder::LittleEndian => le_i32(input)?,
                    TIFFByteOrder::BigEndian => be_i32(input)?,
                };
                let (input, denominator) = match byte_order {
                    TIFFByteOrder::LittleEndian => le_i32(input)?,
                    TIFFByteOrder::BigEndian => be_i32(input)?,
                };
                Ok((input, TIFFData::SRational(numerator, denominator)))
            }
            11 => {
                // FLOAT
                let (input, value) = match byte_order {
                    TIFFByteOrder::LittleEndian => le_f32(input)?,
                    TIFFByteOrder::BigEndian => be_f32(input)?,
                };
                Ok((input, TIFFData::Float(value)))
            }
            12 => {
                // DOUBLE
                let (input, value) = match byte_order {
                    TIFFByteOrder::LittleEndian => le_f64(input)?,
                    TIFFByteOrder::BigEndian => be_f64(input)?,
                };
                Ok((input, TIFFData::Double(value)))
            }
            _ => unreachable!(),
        };
        Ok((input, TIFFIFDEntry { tag, data_type, count, value }))
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = &args[1];
    let file = File::open(Path::new(file_path)).expect("Failed to open file");
    let mut reader = BufReader::new(file);
    let mut buffer = Vec::new();
    reader.read_to_end(&mut buffer).expect("Failed to read file");
    let (input, tiff_header) = TIFFHeader::parse(&buffer).expect("Failed to parse TIFF header");
    let (input, ifd_entries) = length_data(
        |input| {
            let (input, count) = be_u16(input)?;
            take(count as usize)(input)
        },
        |input| TIFFIFDEntry::parse(input, tiff_header.byte_order),
    )(input)
    .expect("Failed to parse IFD entries");
    println!("{:?}", tiff_header);
    println!("{:?}", ifd_entries);
}

fn le_u16(input: &[u8]) -> IResult<&[u8], u16> {
    let (input, value) = take(2usize)(input)?;
    let value = ((value[1] as u16) << 8) | (value[0] as u16);
    Ok((input, value))
}

fn le_u32(input: &[u8]) -> IResult<&[u8], u32> {
    let (input, value) = take(4usize)(input)?;
    let value = ((value[3] as u32) << 24) | ((value[2] as u32) << 16) | ((value[1] as u32) << 8) | (value[0] as u32);
    Ok((input, value))
}

fn le_i16(input: &[u8]) -> IResult<&[u8], i16> {
    let (input, value) = take(2usize)(input)?;
    let value = ((value[1] as i16) << 8) | (value[0] as i16);
    Ok((input, value))
}

fn le_i32(input: &[u8]) -> IResult<&[u8], i32> {
    let (input, value) = take(4usize)(input)?;
    let value = ((value[3] as i32) << 24) | ((value[2] as i32) << 16) | ((value[1] as i32) << 8) | (value[0] as i32);
    Ok((input, value))
}

fn le_f32(input: &[u8]) -> IResult<&[u8], f32> {
    let (input, value) = take(4usize)(input)?;
    let value = f32::from_le_bytes([value[0], value[1], value[2], value[3]]);
    Ok((input, value))
}

fn le_f64(input: &[u8]) -> IResult<&[u8], f64> {
    let (input, value) = take(8usize)(input)?;
    let value = f64::from_le_bytes([value[0], value[1], value[2], value[3], value[4], value[5], value[6], value[7]]);
    Ok((input, value))
}