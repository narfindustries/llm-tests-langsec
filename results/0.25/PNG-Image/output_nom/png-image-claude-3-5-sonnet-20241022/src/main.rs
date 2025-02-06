use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u32, be_u8, be_u16},
    IResult,
    sequence::tuple,
    multi::many0,
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
    CHRM(CHRMData),
    GAMA(u32),
    ICCP(ICCPData),
    SBIT(Vec<u8>),
    SRGB(u8),
    TEXT(TextData),
    ZTXT(ZTXTData),
    ITXT(ITXTData),
    BKGD(Vec<u8>),
    HIST(Vec<u16>),
    PHYS(PHYSData),
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
    entries: Vec<u8>,
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

fn parse_png(input: &[u8]) -> IResult<&[u8], PNG> {
    let (input, signature) = take(8usize)(input)?;
    let (input, chunks) = many0(parse_chunk)(input)?;
    
    Ok((input, PNG {
        signature: signature.try_into().unwrap(),
        chunks,
    }))
}

fn parse_chunk(input: &[u8]) -> IResult<&[u8], Chunk> {
    let (input, length) = be_u32(input)?;
    let (input, chunk_type) = take(4usize)(input)?;
    let (input, data) = take(length)(input)?;
    let (input, crc) = be_u32(input)?;

    let chunk_type_array: [u8; 4] = chunk_type.try_into().unwrap();
    let chunk_type_str = std::str::from_utf8(&chunk_type_array).unwrap();
    
    let parsed_data = match chunk_type_str {
        "IHDR" => parse_ihdr(data).map(|(_, d)| ChunkData::IHDR(d)),
        "PLTE" => parse_plte(data).map(|(_, d)| ChunkData::PLTE(d)),
        "IDAT" => Ok(ChunkData::IDAT(data.to_vec())),
        "IEND" => Ok(ChunkData::IEND),
        "tRNS" => Ok(ChunkData::TRNS(data.to_vec())),
        "cHRM" => parse_chrm(data).map(|(_, d)| ChunkData::CHRM(d)),
        "gAMA" => be_u32(data).map(|(_, d)| ChunkData::GAMA(d)),
        "iCCP" => parse_iccp(data).map(|(_, d)| ChunkData::ICCP(d)),
        "sBIT" => Ok(ChunkData::SBIT(data.to_vec())),
        "sRGB" => be_u8(data).map(|(_, d)| ChunkData::SRGB(d)),
        "tEXt" => parse_text(data).map(|(_, d)| ChunkData::TEXT(d)),
        "zTXt" => parse_ztxt(data).map(|(_, d)| ChunkData::ZTXT(d)),
        "iTXt" => parse_itxt(data).map(|(_, d)| ChunkData::ITXT(d)),
        "bKGD" => Ok(ChunkData::BKGD(data.to_vec())),
        "hIST" => parse_hist(data).map(|(_, d)| ChunkData::HIST(d)),
        "pHYs" => parse_phys(data).map(|(_, d)| ChunkData::PHYS(d)),
        "sPLT" => parse_splt(data).map(|(_, d)| ChunkData::SPLT(d)),
        "tIME" => parse_time(data).map(|(_, d)| ChunkData::TIME(d)),
        _ => Ok(ChunkData::Unknown(data.to_vec())),
    }.unwrap();

    Ok((input, Chunk {
        length,
        chunk_type: chunk_type_array,
        data: parsed_data,
        crc,
    }))
}

fn parse_ihdr(input: &[u8]) -> IResult<&[u8], IHDRData> {
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

fn parse_plte(input: &[u8]) -> IResult<&[u8], Vec<RGB>> {
    let mut result = Vec::new();
    let mut remaining = input;

    while remaining.len() >= 3 {
        let (input, (r, g, b)) = tuple((be_u8, be_u8, be_u8))(remaining)?;
        result.push(RGB { r, g, b });
        remaining = input;
    }

    Ok((remaining, result))
}

fn parse_chrm(input: &[u8]) -> IResult<&[u8], CHRMData> {
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

fn parse_iccp(input: &[u8]) -> IResult<&[u8], ICCPData> {
    let null_pos = input.iter().position(|&x| x == 0).unwrap();
    let (input, name_bytes) = take(null_pos)(input)?;
    let (input, _) = tag(&[0])(input)?;
    let (input, compression_method) = be_u8(input)?;
    
    Ok((input, ICCPData {
        name: String::from_utf8_lossy(name_bytes).to_string(),
        compression_method,
        compressed_profile: input.to_vec(),
    }))
}

fn parse_text(input: &[u8]) -> IResult<&[u8], TextData> {
    let null_pos = input.iter().position(|&x| x == 0).unwrap();
    let (input, keyword_bytes) = take(null_pos)(input)?;
    let (input, _) = tag(&[0])(input)?;
    
    Ok((&[], TextData {
        keyword: String::from_utf8_lossy(keyword_bytes).to_string(),
        text: String::from_utf8_lossy(input).to_string(),
    }))
}

fn parse_ztxt(input: &[u8]) -> IResult<&[u8], ZTXTData> {
    let null_pos = input.iter().position(|&x| x == 0).unwrap();
    let (input, keyword_bytes) = take(null_pos)(input)?;
    let (input, _) = tag(&[0])(input)?;
    let (input, compression_method) = be_u8(input)?;
    
    Ok((&[], ZTXTData {
        keyword: String::from_utf8_lossy(keyword_bytes).to_string(),
        compression_method,
        compressed_text: input.to_vec(),
    }))
}

fn parse_itxt(input: &[u8]) -> IResult<&[u8], ITXTData> {
    let null_pos = input.iter().position(|&x| x == 0).unwrap();
    let (input, keyword_bytes) = take(null_pos)(input)?;
    let (input, _) = tag(&[0])(input)?;
    let (input, compression_flag) = be_u8(input)?;
    let (input, compression_method) = be_u8(input)?;
    
    let null_pos = input.iter().position(|&x| x == 0).unwrap();
    let (input, language_tag_bytes) = take(null_pos)(input)?;
    let (input, _) = tag(&[0])(input)?;
    
    let null_pos = input.iter().position(|&x| x == 0).unwrap();
    let (input, translated_keyword_bytes) = take(null_pos)(input)?;
    let (input, _) = tag(&[0])(input)?;
    
    Ok((&[], ITXTData {
        keyword: String::from_utf8_lossy(keyword_bytes).to_string(),
        compression_flag,
        compression_method,
        language_tag: String::from_utf8_lossy(language_tag_bytes).to_string(),
        translated_keyword: String::from_utf8_lossy(translated_keyword_bytes).to_string(),
        text: String::from_utf8_lossy(input).to_string(),
    }))
}

fn parse_hist(input: &[u8]) -> IResult<&[u8], Vec<u16>> {
    let mut result = Vec::new();
    let mut remaining = input;

    while remaining.len() >= 2 {
        let (input, value) = be_u16(remaining)?;
        result.push(value);
        remaining = input;
    }

    Ok((remaining, result))
}

fn parse_phys(input: &[u8]) -> IResult<&[u8], PHYSData> {
    let (input, (pixels_per_unit_x, pixels_per_unit_y, unit_specifier)) =
        tuple((be_u32, be_u32, be_u8))(input)?;

    Ok((input, PHYSData {
        pixels_per_unit_x,
        pixels_per_unit_y,
        unit_specifier,
    }))
}

fn parse_splt(input: &[u8]) -> IResult<&[u8], SPLTData> {
    let null_pos = input.iter().position(|&x| x == 0).unwrap();
    let (input, name_bytes) = take(null_pos)(input)?;
    let (input, _) = tag(&[0])(input)?;
    let (input, sample_depth) = be_u8(input)?;
    
    Ok((&[], SPLTData {
        name: String::from_utf8_lossy(name_bytes).to_string(),
        sample_depth,
        entries: input.to_vec(),
    }))
}

fn parse_time(input: &[u8]) -> IResult<&[u8], TIMEData> {
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
        eprintln!("Usage: {} <png-file>", args[0]);
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