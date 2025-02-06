use nom::{
    branch::alt,
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    error::{ErrorKind, nom::error::Error},
    multi::{many_m_n},
    number::complete::{be_u16, be_u32, be_u8},
    IResult,
};
use std::{env, fs::File, io::Read};

#[derive(Debug, PartialEq)]
enum ByteOrder {
    Intel,
    Motorola,
}

impl ByteOrder {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        alt((tag("II"), tag("MM")))(input)
            .map(|(input, byte_order)| match byte_order {
                b"II" => ByteOrder::Intel,
                b"MM" => ByteOrder::Motorola,
                _ => unreachable!(),
            })
            .map_err(|e| Error::from_error_kind(input, e.error_kind()))
    }
}

#[derive(Debug, PartialEq)]
enum Tag {
    NewSubfileType,
    SubfileType,
    ImageWidth,
    ImageLength,
    BitsPerSample,
    Compression,
    PhotometricInterpretation,
    Thresholding,
    CellWidth,
    CellLength,
    FillOrder,
    DocumentName,
    ImageDescription,
    Make,
    Model,
    StripOffsets,
    Orientation,
    SamplesPerPixel,
    RowsPerStrip,
    StripByteCounts,
    MinSampleValue,
    MaxSampleValue,
    XResolution,
    YResolution,
    PlanarConfiguration,
    PageName,
    XPosition,
    YPosition,
    FreeOffsets,
    FreeByteCounts,
    GrayResponseUnit,
    GrayResponseCurve,
    T4Options,
    T6Options,
    ResolutionUnit,
    PageNumber,
    ColorResponseUnit,
    ColorResponseCurve,
    Software,
    DateTime,
    Artist,
    HostComputer,
    Predictor,
    WhitePoint,
    PrimaryChromaticities,
    ColorMap,
    HalftoneHints,
    TileWidth,
    TileLength,
    TileOffsets,
    TileByteCounts,
    BadFaxLines,
    CleanFaxData,
    ConsecutiveBadFaxLines,
    Subalgorithm,
    InkSet,
    InkNames,
    NumberOfInks,
    DotRange,
    TargetPrinter,
    ExtraSamples,
    SampleFormat,
    SMinSampleValue,
    SMaxSampleValue,
    TransferRange,
}

impl Tag {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        map(
            be_u16,
            |tag_number: u16| match tag_number {
                254 => Tag::NewSubfileType,
                255 => Tag::SubfileType,
                256 => Tag::ImageWidth,
                257 => Tag::ImageLength,
                258 => Tag::BitsPerSample,
                259 => Tag::Compression,
                262 => Tag::PhotometricInterpretation,
                263 => Tag::Thresholding,
                264 => Tag::CellWidth,
                265 => Tag::CellLength,
                266 => Tag::FillOrder,
                269 => Tag::DocumentName,
                270 => Tag::ImageDescription,
                271 => Tag::Make,
                272 => Tag::Model,
                273 => Tag::StripOffsets,
                274 => Tag::Orientation,
                277 => Tag::SamplesPerPixel,
                278 => Tag::RowsPerStrip,
                279 => Tag::StripByteCounts,
                280 => Tag::MinSampleValue,
                281 => Tag::MaxSampleValue,
                282 => Tag::XResolution,
                283 => Tag::YResolution,
                284 => Tag::PlanarConfiguration,
                285 => Tag::PageName,
                286 => Tag::XPosition,
                287 => Tag::YPosition,
                288 => Tag::FreeOffsets,
                289 => Tag::FreeByteCounts,
                290 => Tag::GrayResponseUnit,
                291 => Tag::GrayResponseCurve,
                292 => Tag::T4Options,
                293 => Tag::T6Options,
                296 => Tag::ResolutionUnit,
                297 => Tag::PageNumber,
                300 => Tag::ColorResponseUnit,
                301 => Tag::ColorResponseCurve,
                305 => Tag::Software,
                306 => Tag::DateTime,
                315 => Tag::Artist,
                316 => Tag::HostComputer,
                317 => Tag::Predictor,
                318 => Tag::WhitePoint,
                319 => Tag::PrimaryChromaticities,
                320 => Tag::ColorMap,
                321 => Tag::HalftoneHints,
                322 => Tag::TileWidth,
                323 => Tag::TileLength,
                324 => Tag::TileOffsets,
                325 => Tag::TileByteCounts,
                326 => Tag::BadFaxLines,
                327 => Tag::CleanFaxData,
                328 => Tag::ConsecutiveBadFaxLines,
                330 => Tag::Subalgorithm,
                332 => Tag::InkSet,
                333 => Tag::InkNames,
                334 => Tag::NumberOfInks,
                335 => Tag::DotRange,
                336 => Tag::TargetPrinter,
                337 => Tag::ExtraSamples,
                338 => Tag::SampleFormat,
                339 => Tag::SMinSampleValue,
                340 => Tag::SMaxSampleValue,
                341 => Tag::TransferRange,
                _ => panic!("Unknown tag number"),
            },
        )(input)
    }
}

#[derive(Debug, PartialEq)]
enum FieldType {
    Byte,
    Ascii,
    BINARY,
    Long,
    Rational,
}

impl FieldType {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        map(
            be_u16,
            |field_type: u16| match field_type {
                1 => FieldType::Byte,
                2 => FieldType::Ascii,
                3 => FieldType::BINARY,
                4 => FieldType::Long,
                5 => FieldType::Rational,
                _ => panic!("Unknown field type"),
            },
        )(input)
    }
}

#[derive(Debug, PartialEq)]
struct IFD {
    entries: Vec<IFDEntry>,
}

impl IFD {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, num_entries) = be_u16(input)?;
        let (input, entries) = many_m_n(num_entries as usize, num_entries as usize, IFDEntry::parse)(input)?;
        Ok((input, IFD { entries }))
    }
}

#[derive(Debug, PartialEq)]
struct IFDEntry {
    tag: Tag,
    field_type: FieldType,
    count: u32,
    value_offset: u32,
}

impl IFDEntry {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, tag) = Tag::parse(input)?;
        let (input, field_type) = FieldType::parse(input)?;
        let (input, count) = be_u32(input)?;
        let (input, value_offset) = be_u32(input)?;
        Ok((input, IFDEntry { tag, field_type, count, value_offset }))
    }
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

impl PhotometricInterpretation {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        map_res(be_u8, |photometric_interpretation: u8| match photometric_interpretation {
            0 => Ok(PhotometricInterpretation::WhiteIsZero),
            1 => Ok(PhotometricInterpretation::BlackIsZero),
            2 => Ok(PhotometricInterpretation::RGB),
            3 => Ok(PhotometricInterpretation::PaletteColor),
            4 => Ok(PhotometricInterpretation::TransparencyMask),
            5 => Ok(PhotometricInterpretation::CMYK),
            6 => Ok(PhotometricInterpretation::YCbCr),
            _ => Err(Error::from_error_kind(input, ErrorKind::AlphaNumeric)),
        })(input)
    }
}

#[derive(Debug, PartialEq)]
enum Compression {
    NoCompression,
    CCITTGroup3,
    CCITTGroup4,
    LZW,
    PackBits,
    JPEG,
}

impl Compression {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        map_res(be_u16, |compression: u16| match compression {
            1 => Ok(Compression::NoCompression),
            2 => Ok(Compression::CCITTGroup3),
            3 => Ok(Compression::CCITTGroup4),
            4 => Ok(Compression::LZW),
            5 => Ok(Compression::PackBits),
            6 => Ok(Compression::JPEG),
            _ => Err(Error::from_error_kind(input, ErrorKind::AlphaNumeric)),
        })(input)
    }
}

#[derive(Debug, PartialEq)]
enum FillOrder {
    MSBToLSB,
    LSBToMSB,
}

impl FillOrder {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        map_res(be_u16, |fill_order: u16| match fill_order {
            1 => Ok(FillOrder::MSBToLSB),
            2 => Ok(FillOrder::LSBToMSB),
            _ => Err(Error::from_error_kind(input, ErrorKind::AlphaNumeric)),
        })(input)
    }
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

impl Orientation {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        map_res(be_u16, |orientation: u16| match orientation {
            1 => Ok(Orientation::TopLeft),
            2 => Ok(Orientation::TopRight),
            3 => Ok(Orientation::BottomRight),
            4 => Ok(Orientation::BottomLeft),
            5 => Ok(Orientation::LeftTop),
            6 => Ok(Orientation::RightTop),
            7 => Ok(Orientation::RightBottom),
            8 => Ok(Orientation::LeftBottom),
            _ => Err(Error::from_error_kind(input, ErrorKind::AlphaNumeric)),
        })(input)
    }
}

#[derive(Debug, PartialEq)]
enum PlanarConfiguration {
    Chunky,
    Planar,
}

impl PlanarConfiguration {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        map_res(be_u16, |planar_configuration: u16| match planar_configuration {
            1 => Ok(PlanarConfiguration::Chunky),
            2 => Ok(PlanarConfiguration::Planar),
            _ => Err(Error::from_error_kind(input, ErrorKind::AlphaNumeric)),
        })(input)
    }
}

#[derive(Debug, PartialEq)]
enum ResolutionUnit {
    None,
    Inch,
    Centimeter,
}

impl ResolutionUnit {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        map_res(be_u16, |resolution_unit: u16| match resolution_unit {
            1 => Ok(ResolutionUnit::None),
            2 => Ok(ResolutionUnit::Inch),
            3 => Ok(ResolutionUnit::Centimeter),
            _ => Err(Error::from_error_kind(input, ErrorKind::AlphaNumeric)),
        })(input)
    }
}

#[derive(Debug, PartialEq)]
enum Predictor {
    NoPrediction,
    HorizontalDifferencing,
}

impl Predictor {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        map_res(be_u16, |predictor: u16| match predictor {
            1 => Ok(Predictor::NoPrediction),
            2 => Ok(Predictor::HorizontalDifferencing),
            _ => Err(Error::from_error_kind(input, ErrorKind::AlphaNumeric)),
        })(input)
    }
}

#[derive(Debug, PartialEq)]
enum InkSet {
    CMYK,
    NotCMYK,
}

impl InkSet {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        map_res(be_u16, |ink_set: u16| match ink_set {
            1 => Ok(InkSet::CMYK),
            2 => Ok(InkSet::NotCMYK),
            _ => Err(Error::from_error_kind(input, ErrorKind::AlphaNumeric)),
        })(input)
    }
}

fn tiff_file(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _byte_order) = ByteOrder::parse(input)?;
    let (input, _) = tag("42")(input)?;
    let (input, ifd_offset) = be_u32(input)?;
    let (input, _) = take(ifd_offset)(input)?;
    let (input, _) = IFD::parse(input)?;
    Ok((input, ()))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];
    let mut file = File::open(filename).expect("Couldn't open file");
    let mut data = Vec::new();
    file.read_to_end(&mut data).expect("Failed to read file");
    match tiff_file(&data) {
        Ok(remaining) => {
            println!("Parsed TIFF file successfully");
            println!("Remaining data: {:?}", remaining);
        }
        Err(error) => {
            println!("Error parsing TIFF file: {:?}", error);
        }
    }
}