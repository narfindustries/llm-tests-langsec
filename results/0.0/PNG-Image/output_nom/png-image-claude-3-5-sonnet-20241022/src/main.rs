use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u32, be_u8, be_u16},
    IResult,
    sequence::tuple,
    multi::many0,
    error::Error,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct PNG {
    signature: [u8; 8],
    chunks: Vec<Chunk>,
}

#[derive(Debug)]
struct Chunk {
    length: u32,
    chunk_type: [u8; 4],
    data: ChunkData,
    crc: u32,
}

#[derive(Debug)]
enum ChunkData {
    IHDR(IHDRData),
    PLTE(Vec<RGB>),
    IDAT(Vec<u8>),
    IEND,
    TRNS(Vec<u8>),
    GAMA(u32),
    CHRM(CHRMData),
    SRGB(u8),
    ICCP(ICCPData),
    TEXT(TextData),
    ZTXT(ZTXTData),
    ITXT(ITXTData),
    BKGD(Vec<u8>),
    PHYS(PHYSData),
    SBIT(Vec<u8>),
    SPLT(SPLTData),
    TIME(TIMEData),
    Unknown(Vec<u8>),
}

#[derive(Debug)]
struct IHDRData {
    width: u32,
    height: u32,
    bit_depth: u8,
    color_type: u8,
    compression_method: u8,
    filter_method: u8,
    interlace_method: u8,
}

#[derive(Debug)]
struct RGB {
    r: u8,
    g: u8,
    b: u8,
}

#[derive(Debug)]
struct CHRMData {
    white_point_x: u32,
    white_point_y: u32,
    red_x: u32,
    red_y: u32,
    green_x: u32,
    green_y: u32,
    blue_x: u32,
    blue_y: u32,
}

#[derive(Debug)]
struct ICCPData {
    name: String,
    compression_method: u8,
    compressed_profile: Vec<u8>,
}

#[derive(Debug)]
struct TextData {
    keyword: String,
    text: String,
}

#[derive(Debug)]
struct ZTXTData {
    keyword: String,
    compression_method: u8,
    compressed_text: Vec<u8>,
}

#[derive(Debug)]
struct ITXTData {
    keyword: String,
    compression_flag: u8,
    compression_method: u8,
    language_tag: String,
    translated_keyword: String,
    text: String,
}

#[derive(Debug)]
struct PHYSData {
    pixels_per_unit_x: u32,
    pixels_per_unit_y: u32,
    unit_specifier: u8,
}

#[derive(Debug)]
struct SPLTData {
    name: String,
    sample_depth: u8,
    entries: Vec<SPLTEntry>,
}

#[derive(Debug)]
struct SPLTEntry {
    red: u16,
    green: u16,
    blue: u16,
    alpha: u16,
    frequency: u16,
}

#[derive(Debug)]
struct TIMEData {
    year: u16,
    month: u8,
    day: u8,
    hour: u8,
    minute: u8,
    second: u8,
}

fn parse_png(input: &[u8]) -> IResult<&[u8], PNG, Error<&[u8]>> {
    let (input, signature) = take(8usize)(input)?;
    let (input, chunks) = many0(parse_chunk)(input)?;
    
    Ok((input, PNG {
        signature: signature.try_into().unwrap(),
        chunks,
    }))
}

fn parse_chunk(input: &[u8]) -> IResult<&[u8], Chunk, Error<&[u8]>> {
    let (input, length) = be_u32(input)?;
    let (input, chunk_type) = take(4usize)(input)?;
    let (input, data) = take(length)(input)?;
    let (input, crc) = be_u32(input)?;

    let chunk_type_array: [u8; 4] = chunk_type.try_into().unwrap();
    let chunk_type_str = std::str::from_utf8(&chunk_type_array).unwrap();

    let chunk_data = match chunk_type_str {
        "IHDR" => ChunkData::IHDR(parse_ihdr(data).unwrap().1),
        "PLTE" => ChunkData::PLTE(parse_plte(data).unwrap().1),
        "IDAT" => ChunkData::IDAT(data.to_vec()),
        "IEND" => ChunkData::IEND,
        "tRNS" => ChunkData::TRNS(data.to_vec()),
        "gAMA" => ChunkData::GAMA(be_u32::<&[u8], Error<&[u8]>>(data).unwrap().1),
        "cHRM" => ChunkData::CHRM(parse_chrm(data).unwrap().1),
        "sRGB" => ChunkData::SRGB(data[0]),
        "iCCP" => ChunkData::ICCP(parse_iccp(data).unwrap().1),
        "tEXt" => ChunkData::TEXT(parse_text(data).unwrap().1),
        "zTXt" => ChunkData::ZTXT(parse_ztxt(data).unwrap().1),
        "iTXt" => ChunkData::ITXT(parse_itxt(data).unwrap().1),
        "bKGD" => ChunkData::BKGD(data.to_vec()),
        "pHYs" => ChunkData::PHYS(parse_phys(data).unwrap().1),
        "sBIT" => ChunkData::SBIT(data.to_vec()),
        "sPLT" => ChunkData::SPLT(parse_splt(data).unwrap().1),
        "tIME" => ChunkData::TIME(parse_time(data).unwrap().1),
        _ => ChunkData::Unknown(data.to_vec()),
    };

    Ok((input, Chunk {
        length,
        chunk_type: chunk_type_array,
        data: chunk_data,
        crc,
    }))
}

fn parse_ihdr(input: &[u8]) -> IResult<&[u8], IHDRData, Error<&[u8]>> {
    let (input, (width, height, bit_depth, color_type, compression_method, filter_method, interlace_method)) =
        tuple((be_u32, be_u32, be_u8, be_u8, be_u8, be_u8, be_u8))(input)?;

    Ok((input, IHDRData {
        width,
        height,
        bit_depth,
        color_type,
        compression_method,
        filter_method,
        interlace_method,
    }))
}

fn parse_plte(input: &[u8]) -> IResult<&[u8], Vec<RGB>, Error<&[u8]>> {
    let mut result = Vec::new();
    let mut remaining = input;

    while remaining.len() >= 3 {
        let (input, (r, g, b)) = tuple((be_u8, be_u8, be_u8))(remaining)?;
        result.push(RGB { r, g, b });
        remaining = input;
    }

    Ok((remaining, result))
}

fn parse_chrm(input: &[u8]) -> IResult<&[u8], CHRMData, Error<&[u8]>> {
    let (input, (white_point_x, white_point_y, red_x, red_y, green_x, green_y, blue_x, blue_y)) =
        tuple((be_u32, be_u32, be_u32, be_u32, be_u32, be_u32, be_u32, be_u32))(input)?;

    Ok((input, CHRMData {
        white_point_x,
        white_point_y,
        red_x,
        red_y,
        green_x,
        green_y,
        blue_x,
        blue_y,
    }))
}

fn parse_null_terminated_string(input: &[u8]) -> IResult<&[u8], String, Error<&[u8]>> {
    let mut end = 0;
    while end < input.len() && input[end] != 0 {
        end += 1;
    }
    let (remaining, string_bytes) = take(end)(input)?;
    let (remaining, _) = tag(&[0u8])(remaining)?;
    Ok((remaining, String::from_utf8_lossy(string_bytes).into_owned()))
}

fn parse_iccp(input: &[u8]) -> IResult<&[u8], ICCPData, Error<&[u8]>> {
    let (input, name) = parse_null_terminated_string(input)?;
    let (input, compression_method) = be_u8(input)?;
    Ok((input, ICCPData {
        name,
        compression_method,
        compressed_profile: input.to_vec(),
    }))
}

fn parse_text(input: &[u8]) -> IResult<&[u8], TextData, Error<&[u8]>> {
    let (input, keyword) = parse_null_terminated_string(input)?;
    Ok((&[], TextData {
        keyword,
        text: String::from_utf8_lossy(input).into_owned(),
    }))
}

fn parse_ztxt(input: &[u8]) -> IResult<&[u8], ZTXTData, Error<&[u8]>> {
    let (input, keyword) = parse_null_terminated_string(input)?;
    let (input, compression_method) = be_u8(input)?;
    Ok((&[], ZTXTData {
        keyword,
        compression_method,
        compressed_text: input.to_vec(),
    }))
}

fn parse_itxt(input: &[u8]) -> IResult<&[u8], ITXTData, Error<&[u8]>> {
    let (input, keyword) = parse_null_terminated_string(input)?;
    let (input, compression_flag) = be_u8(input)?;
    let (input, compression_method) = be_u8(input)?;
    let (input, language_tag) = parse_null_terminated_string(input)?;
    let (input, translated_keyword) = parse_null_terminated_string(input)?;
    Ok((&[], ITXTData {
        keyword,
        compression_flag,
        compression_method,
        language_tag,
        translated_keyword,
        text: String::from_utf8_lossy(input).into_owned(),
    }))
}

fn parse_phys(input: &[u8]) -> IResult<&[u8], PHYSData, Error<&[u8]>> {
    let (input, (pixels_per_unit_x, pixels_per_unit_y, unit_specifier)) =
        tuple((be_u32, be_u32, be_u8))(input)?;

    Ok((input, PHYSData {
        pixels_per_unit_x,
        pixels_per_unit_y,
        unit_specifier,
    }))
}

fn parse_splt(input: &[u8]) -> IResult<&[u8], SPLTData, Error<&[u8]>> {
    let (input, name) = parse_null_terminated_string(input)?;
    let (input, sample_depth) = be_u8(input)?;
    
    let mut entries = Vec::new();
    let mut remaining = input;
    
    while remaining.len() >= 10 {
        let (input, (red, green, blue, alpha, frequency)) =
            tuple((be_u16, be_u16, be_u16, be_u16, be_u16))(remaining)?;
        entries.push(SPLTEntry {
            red,
            green,
            blue,
            alpha,
            frequency,
        });
        remaining = input;
    }

    Ok((remaining, SPLTData {
        name,
        sample_depth,
        entries,
    }))
}

fn parse_time(input: &[u8]) -> IResult<&[u8], TIMEData, Error<&[u8]>> {
    let (input, (year, month, day, hour, minute, second)) =
        tuple((be_u16, be_u8, be_u8, be_u8, be_u8, be_u8))(input)?;

    Ok((input, TIMEData {
        year,
        month,
        day,
        hour,
        minute,
        second,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <png_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_png(&buffer) {
        Ok((_, png)) => println!("{:#?}", png),
        Err(e) => eprintln!("Failed to parse PNG: {:?}", e),
    }
}