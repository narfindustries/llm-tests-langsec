use std::env;
use std::fs::File;
use std::io::Read;
use nom::bytes::complete::{tag, take};
use nom::combinator::verify;
use nom::number::complete::{be_u32};
use nom::IResult;

const PNG_MAGIC_NUMBER: [u8; 8] = [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A];

#[derive(Debug, PartialEq)]
enum ColorType {
    Grayscale,
    RGB,
    PLTE,
    GreyscaleAlpha,
    RGBA,
}

#[derive(Debug, PartialEq)]
enum CompressionMethod {
    DeflateInflate,
}

#[derive(Debug, PartialEq)]
enum FilterMethod {
    Adaptive,
}

#[derive(Debug, PartialEq)]
enum InterlaceMethod {
    NoInterlacing,
    Adam7,
}

#[derive(Debug, PartialEq)]
struct IhdrChunk {
    width: u32,
    height: u32,
    bit_depth: u8,
    color_type: ColorType,
    compression_method: CompressionMethod,
    filter_method: FilterMethod,
    interlace_method: InterlaceMethod,
}

impl IhdrChunk {
    fn parse(input: &[u8]) -> IResult<&[u8], IhdrChunk> {
        let (input, width) = be_u32(input)?;
        let (input, height) = be_u32(input)?;
        let (input, bit_depth) = take(1u8)(input)?;
        let (input, color_type) = take(1u8)(input)?;
        let (input, compression_method) = take(1u8)(input)?;
        let (input, filter_method) = take(1u8)(input)?;
        let (input, interlace_method) = take(1u8)(input)?;

        let color_type = match color_type[0] {
            0 => ColorType::Grayscale,
            2 => ColorType::RGB,
            3 => ColorType::PLTE,
            4 => ColorType::GreyscaleAlpha,
            6 => ColorType::RGBA,
            _ => panic!("Invalid color type"),
        };

        let compression_method = match compression_method[0] {
            0 => CompressionMethod::DeflateInflate,
            _ => panic!("Invalid compression method"),
        };

        let filter_method = match filter_method[0] {
            0 => FilterMethod::Adaptive,
            _ => panic!("Invalid filter method"),
        };

        let interlace_method = match interlace_method[0] {
            0 => InterlaceMethod::NoInterlacing,
            1 => InterlaceMethod::Adam7,
            _ => panic!("Invalid interlace method"),
        };

        Ok((input, IhdrChunk {
            width,
            height,
            bit_depth: bit_depth[0],
            color_type,
            compression_method,
            filter_method,
            interlace_method,
        }))
    }
}

#[derive(Debug, PartialEq)]
struct PngImage {
    magic_number: [u8; 8],
    ihdr_chunk: IhdrChunk,
}

impl PngImage {
    fn parse(input: &[u8]) -> IResult<&[u8], PngImage> {
        let (input, magic_number) = verify(take(8u8), |x| {
            x == &PNG_MAGIC_NUMBER[..]
        })(input)?;

        let (input, _) = tag("IHDR")(input)?;
        let (input, ihdr_length) = be_u32(input)?;
        let (input, ihdr_chunk_data) = take(ihdr_length)(input)?;
        let (_, ihdr_chunk) = IhdrChunk::parse(ihdr_chunk_data)?;

        Ok((input, PngImage {
            magic_number: [
                magic_number[0], magic_number[1], magic_number[2], magic_number[3],
                magic_number[4], magic_number[5], magic_number[6], magic_number[7],
            ],
            ihdr_chunk,
        }))
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <png_file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).unwrap();
    let mut data = Vec::new();
    file.read_to_end(&mut data).unwrap();

    let result = PngImage::parse(&data);
    match result {
        Ok((_, png_image)) => {
            println!("{:?}", png_image);
        }
        Err(err) => {
            println!("{:?}", err);
        }
    }
}