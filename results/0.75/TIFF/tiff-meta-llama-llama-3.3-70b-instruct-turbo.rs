use nom::{
    bytes::complete::{tag, take},
    combinator::{map, verify},
    multi::{many_till, separated_list1},
    number::complete::{be_u16, be_u32, be_u8},
    sequence::{tuple, preceded},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
    path::Path,
};

#[derive(Debug)]
enum PhotometricInterpretation {
    WhiteIsZero,
    BlackIsZero,
    RGB,
    PaletteColor,
    TransparencyMask,
    CMYK,
    YCbCr,
    // Add more variants as needed
}

impl PhotometricInterpretation {
    fn from_u16(value: u16) -> Option<Self> {
        match value {
            0 => Some(PhotometricInterpretation::WhiteIsZero),
            1 => Some(PhotometricInterpretation::BlackIsZero),
            2 => Some(PhotometricInterpretation::RGB),
            3 => Some(PhotometricInterpretation::PaletteColor),
            4 => Some(PhotometricInterpretation::TransparencyMask),
            5 => Some(PhotometricInterpretation::CMYK),
            6 => Some(PhotometricInterpretation::YCbCr),
            _ => None,
        }
    }
}

#[derive(Debug)]
enum Compression {
    None,
    CCITTGroup3,
    CCITTGroup4,
    LZW,
    // Add more variants as needed
}

impl Compression {
    fn from_u16(value: u16) -> Option<Self> {
        match value {
            1 => Some(Compression::None),
            2 => Some(Compression::CCITTGroup3),
            3 => Some(Compression::CCITTGroup4),
            5 => Some(Compression::LZW),
            _ => None,
        }
    }
}

#[derive(Debug)]
struct IFD {
    tag: u16,
    type_: u16,
    count: u32,
    value: Vec<u8>,
}

impl IFD {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, tag) = be_u16(input)?;
        let (input, type_) = be_u16(input)?;
        let (input, count) = be_u32(input)?;
        let (input, value) = take(count as usize)(input)?;
        Ok((input, IFD { tag, type_, count, value }))
    }
}

#[derive(Debug)]
struct TiffHeader {
    byte_order: [u8; 2],
    magic: u16,
    offset: u32,
}

impl TiffHeader {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, byte_order) = take(2u8)(input)?;
        let (input, magic) = be_u16(input)?;
        let (input, offset) = be_u32(input)?;
        Ok((input, TiffHeader { byte_order, magic, offset }))
    }
}

#[derive(Debug)]
struct Tiff {
    header: TiffHeader,
    ifd: Vec<IFD>,
}

impl Tiff {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, header) = TiffHeader::parse(input)?;
        let (input, ifd_count) = be_u16(input)?;
        let (input, ifd) = many_till(verify(be_u16, |tag| *tag == 0xff), take(4u8))(input)?;
        let mut ifd_list = Vec::new();
        for _ in 0..ifd_count {
            let (input, ifd_item) = IFD::parse(input)?;
            ifd_list.push(ifd_item);
        }
        Ok((input, Tiff { header, ifd: ifd_list }))
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <input_file>", args[0]);
    }
    let path = Path::new(&args[1]);
    let file = File::open(path).expect("Failed to open file");
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input).expect("Failed to read file");
    let (_input, tiff) = Tiff::parse(&input).expect("Failed to parse TIFF");
    println!("{:?}", tiff);
}