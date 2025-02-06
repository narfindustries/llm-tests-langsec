use nom::{
    bytes::complete::{take},
    combinator::{map, map_res},
    multi::{length_data},
    number::complete::{be_u16, be_u32},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug, PartialEq)]
enum ByteOrder {
    LittleEndian,
    BigEndian,
}

#[derive(Debug, PartialEq)]
enum Compression {
    NoCompression,
    CCITTGroup3,
    CCITTGroup4,
    LZW,
    PackBits,
}

#[derive(Debug, PartialEq)]
enum PhotometricInterpretation {
    WhiteIsZero,
    BlackIsZero,
    RGB,
    PaletteColor,
    TransparencyMask,
    CMYK,
    YCbCr,
}

#[derive(Debug, PartialEq)]
enum Orientation {
    TopLeft,
    TopRight,
    BottomRight,
    BottomLeft,
    LeftTop,
    RightTop,
    RightBottom,
    LeftBottom,
}

#[derive(Debug, PartialEq)]
enum PlanarConfiguration {
    Chunky,
    Planar,
}

#[derive(Debug, PartialEq)]
enum YCbCrSubSampling {
    NoSubSampling,
    HorizontalSubSampling,
    VerticalSubSampling,
    BothSubSampling,
}

#[derive(Debug, PartialEq)]
enum YCbCrPositioning {
    CoSited,
    Centered,
}

#[derive(Debug, PartialEq)]
struct IFH {
    byte_order: ByteOrder,
    magic_number: u16,
    offset_to_first_ifd: u32,
}

#[derive(Debug, PartialEq)]
struct IFD {
    number_of_directory_entries: u16,
    directory_entries: Vec<DirectoryEntry>,
    next_ifd_offset: u32,
}

#[derive(Debug, PartialEq)]
struct DirectoryEntry {
    tag: u16,
    data_type: u16,
    count: u32,
    value_or_offset: u32,
}

fn parse_byte_order(input: &[u8]) -> IResult<&[u8], ByteOrder> {
    map_res(take(2usize), |input: &[u8]| match input {
        b"II" => Ok(ByteOrder::LittleEndian),
        b"MM" => Ok(ByteOrder::BigEndian),
        _ => Err("Invalid byte order"),
    })(input)
}

fn parse_magic_number(input: &[u8]) -> IResult<&[u8], u16> {
    map(be_u16, |n: u16| n)(input)
}

fn parse_offset_to_first_ifd(input: &[u8]) -> IResult<&[u8], u32> {
    map(be_u32, |n: u32| n)(input)
}

fn parse_ifh(input: &[u8]) -> IResult<&[u8], IFH> {
    let (input, byte_order) = parse_byte_order(input)?;
    let (input, magic_number) = parse_magic_number(input)?;
    let (input, offset_to_first_ifd) = parse_offset_to_first_ifd(input)?;
    Ok((input, IFH { byte_order, magic_number, offset_to_first_ifd }))
}

fn parse_directory_entry(input: &[u8]) -> IResult<&[u8], DirectoryEntry> {
    let (input, tag) = map(be_u16, |n: u16| n)(input)?;
    let (input, data_type) = map(be_u16, |n: u16| n)(input)?;
    let (input, count) = map(be_u32, |n: u32| n)(input)?;
    let (input, value_or_offset) = map(be_u32, |n: u32| n)(input)?;
    Ok((input, DirectoryEntry { tag, data_type, count, value_or_offset }))
}

fn parse_ifd(input: &[u8]) -> IResult<&[u8], IFD> {
    let (input, number_of_directory_entries) = map(be_u16, |n: u16| n)(input)?;
    let mut directory_entries = Vec::new();
    let mut remaining_input = input;
    for _ in 0..number_of_directory_entries {
        let (input, entry) = parse_directory_entry(remaining_input)?;
        directory_entries.push(entry);
        remaining_input = input;
    }
    let (input, next_ifd_offset) = map(be_u32, |n: u32| n)(remaining_input)?;
    Ok((input, IFD { number_of_directory_entries, directory_entries, next_ifd_offset }))
}

fn parse_image_width(input: &[u8]) -> IResult<&[u8], u16> {
    map(be_u16, |n: u16| n)(input)
}

fn parse_image_length(input: &[u8]) -> IResult<&[u8], u16> {
    map(be_u16, |n: u16| n)(input)
}

fn parse_bits_per_sample(input: &[u8]) -> IResult<&[u8], u16> {
    map(be_u16, |n: u16| n)(input)
}

fn parse_compression(input: &[u8]) -> IResult<&[u8], Compression> {
    map_res(be_u16, |n: u16| match n {
        1 => Ok(Compression::NoCompression),
        2 => Ok(Compression::CCITTGroup3),
        3 => Ok(Compression::CCITTGroup4),
        4 => Ok(Compression::LZW),
        5 => Ok(Compression::PackBits),
        _ => Err("Invalid compression"),
    })(input)
}

fn parse_photometric_interpretation(input: &[u8]) -> IResult<&[u8], PhotometricInterpretation> {
    map_res(be_u16, |n: u16| match n {
        0 => Ok(PhotometricInterpretation::WhiteIsZero),
        1 => Ok(PhotometricInterpretation::BlackIsZero),
        2 => Ok(PhotometricInterpretation::RGB),
        3 => Ok(PhotometricInterpretation::PaletteColor),
        4 => Ok(PhotometricInterpretation::TransparencyMask),
        5 => Ok(PhotometricInterpretation::CMYK),
        6 => Ok(PhotometricInterpretation::YCbCr),
        _ => Err("Invalid photometric interpretation"),
    })(input)
}

fn parse_orientation(input: &[u8]) -> IResult<&[u8], Orientation> {
    map_res(be_u16, |n: u16| match n {
        1 => Ok(Orientation::TopLeft),
        2 => Ok(Orientation::TopRight),
        3 => Ok(Orientation::BottomRight),
        4 => Ok(Orientation::BottomLeft),
        5 => Ok(Orientation::LeftTop),
        6 => Ok(Orientation::RightTop),
        7 => Ok(Orientation::RightBottom),
        8 => Ok(Orientation::LeftBottom),
        _ => Err("Invalid orientation"),
    })(input)
}

fn parse_samples_per_pixel(input: &[u8]) -> IResult<&[u8], u16> {
    map(be_u16, |n: u16| n)(input)
}

fn parse_planar_configuration(input: &[u8]) -> IResult<&[u8], PlanarConfiguration> {
    map_res(be_u16, |n: u16| match n {
        1 => Ok(PlanarConfiguration::Chunky),
        2 => Ok(PlanarConfiguration::Planar),
        _ => Err("Invalid planar configuration"),
    })(input)
}

fn parse_ycbcr_sub_sampling(input: &[u8]) -> IResult<&[u8], YCbCrSubSampling> {
    map_res(be_u16, |n: u16| match n {
        1 => Ok(YCbCrSubSampling::NoSubSampling),
        2 => Ok(YCbCrSubSampling::HorizontalSubSampling),
        3 => Ok(YCbCrSubSampling::VerticalSubSampling),
        4 => Ok(YCbCrSubSampling::BothSubSampling),
        _ => Err("Invalid YCbCr sub-sampling"),
    })(input)
}

fn parse_ycbcr_positioning(input: &[u8]) -> IResult<&[u8], YCbCrPositioning> {
    map_res(be_u16, |n: u16| match n {
        1 => Ok(YCbCrPositioning::CoSited),
        2 => Ok(YCbCrPositioning::Centered),
        _ => Err("Invalid YCbCr positioning"),
    })(input)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return;
    }
    let mut file = File::open(&args[1]).unwrap();
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).unwrap();
    let (input, ifh) = parse_ifh(&buffer).unwrap();
    println!("IFH: {:?}", ifh);
    let (input, ifd) = parse_ifd(input).unwrap();
    println!("IFD: {:?}", ifd);
    for entry in ifd.directory_entries {
        match entry.tag {
            256 => {
                let (_input, image_width) = parse_image_width(&buffer[entry.value_or_offset as usize..]).unwrap();
                println!("Image Width: {}", image_width);
            }
            257 => {
                let (_input, image_length) = parse_image_length(&buffer[entry.value_or_offset as usize..]).unwrap();
                println!("Image Length: {}", image_length);
            }
            258 => {
                let (_input, bits_per_sample) = parse_bits_per_sample(&buffer[entry.value_or_offset as usize..]).unwrap();
                println!("Bits Per Sample: {}", bits_per_sample);
            }
            259 => {
                let (_input, compression) = parse_compression(&buffer[entry.value_or_offset as usize..]).unwrap();
                println!("Compression: {:?}", compression);
            }
            262 => {
                let (_input, photometric_interpretation) = parse_photometric_interpretation(&buffer[entry.value_or_offset as usize..]).unwrap();
                println!("Photometric Interpretation: {:?}", photometric_interpretation);
            }
            274 => {
                let (_input, orientation) = parse_orientation(&buffer[entry.value_or_offset as usize..]).unwrap();
                println!("Orientation: {:?}", orientation);
            }
            277 => {
                let (_input, samples_per_pixel) = parse_samples_per_pixel(&buffer[entry.value_or_offset as usize..]).unwrap();
                println!("Samples Per Pixel: {}", samples_per_pixel);
            }
            284 => {
                let (_input, planar_configuration) = parse_planar_configuration(&buffer[entry.value_or_offset as usize..]).unwrap();
                println!("Planar Configuration: {:?}", planar_configuration);
            }
            530 => {
                let (_input, ycbcr_sub_sampling) = parse_ycbcr_sub_sampling(&buffer[entry.value_or_offset as usize..]).unwrap();
                println!("YCbCr Sub-Sampling: {:?}", ycbcr_sub_sampling);
            }
            531 => {
                let (_input, ycbcr_positioning) = parse_ycbcr_positioning(&buffer[entry.value_or_offset as usize..]).unwrap();
                println!("YCbCr Positioning: {:?}", ycbcr_positioning);
            }
            _ => {}
        }
    }
}