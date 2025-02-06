use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u32, be_u8, be_u16},
    IResult,
    sequence::tuple,
    multi::{many0, count},
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
    PLTE(Vec<PLTEEntry>),
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
    HIST(Vec<u16>),
    TIME(TimeData),
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
struct PLTEEntry {
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
    palette_name: String,
    sample_depth: u8,
    entries: Vec<u8>,
}

#[derive(Debug)]
struct TimeData {
    year: u16,
    month: u8,
    day: u8,
    hour: u8,
    minute: u8,
    second: u8,
}

fn parse_png_signature(input: &[u8]) -> IResult<&[u8], [u8; 8]> {
    let (input, signature) = take(8usize)(input)?;
    Ok((input, signature.try_into().unwrap()))
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

fn parse_plte(input: &[u8], length: u32) -> IResult<&[u8], Vec<PLTEEntry>> {
    let entry_count = length as usize / 3;
    let mut entries = Vec::with_capacity(entry_count);
    
    let (input, raw_entries) = count(tuple((be_u8, be_u8, be_u8)), entry_count)(input)?;
    
    for (r, g, b) in raw_entries {
        entries.push(PLTEEntry { r, g, b });
    }
    
    Ok((input, entries))
}

fn parse_chunk(input: &[u8]) -> IResult<&[u8], Chunk> {
    let (input, length) = be_u32(input)?;
    let (input, chunk_type) = take(4usize)(input)?;
    let chunk_type: [u8; 4] = chunk_type.try_into().unwrap();
    
    let (input, chunk_data) = take(length)(input)?;
    let (input, crc) = be_u32(input)?;
    
    let data = match &chunk_type {
        b"IHDR" => {
            let (_, ihdr) = parse_ihdr(chunk_data)?;
            ChunkData::IHDR(ihdr)
        },
        b"PLTE" => {
            let (_, plte) = parse_plte(chunk_data, length)?;
            ChunkData::PLTE(plte)
        },
        b"IDAT" => ChunkData::IDAT(chunk_data.to_vec()),
        b"IEND" => ChunkData::IEND,
        b"gAMA" => {
            let (_, gamma) = be_u32(chunk_data)?;
            ChunkData::GAMA(gamma)
        },
        _ => ChunkData::Unknown(chunk_data.to_vec()),
    };
    
    Ok((input, Chunk {
        length,
        chunk_type,
        data,
        crc,
    }))
}

fn parse_png(input: &[u8]) -> IResult<&[u8], PNG> {
    let (input, signature) = parse_png_signature(input)?;
    let (input, chunks) = many0(parse_chunk)(input)?;
    
    Ok((input, PNG {
        signature,
        chunks,
    }))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <png_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_png(&buffer) {
        Ok((_, png)) => println!("{:#?}", png),
        Err(e) => eprintln!("Error parsing PNG: {:?}", e),
    }

    Ok(())
}