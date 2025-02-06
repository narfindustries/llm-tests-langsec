use nom::{
    bytes::complete::take,
    number::complete::{be_u32, be_u8, be_u16},
    IResult,
    sequence::tuple,
    multi::many0,
    error::Error,
};
use std::fs::File;
use std::io::Read;
use std::env;

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
    IHDR {
        width: u32,
        height: u32,
        bit_depth: u8,
        color_type: u8,
        compression_method: u8,
        filter_method: u8,
        interlace_method: u8,
    },
    PLTE(Vec<(u8, u8, u8)>),
    IDAT(Vec<u8>),
    IEND,
    TRNS(Vec<u8>),
    CHRM {
        white_point_x: u32,
        white_point_y: u32,
        red_x: u32,
        red_y: u32,
        green_x: u32,
        green_y: u32,
        blue_x: u32,
        blue_y: u32,
    },
    GAMA(u32),
    ICCP {
        profile_name: String,
        compression_method: u8,
        compressed_profile: Vec<u8>,
    },
    SBIT(Vec<u8>),
    SRGB(u8),
    TEXT {
        keyword: String,
        text: String,
    },
    ZTXT {
        keyword: String,
        compression_method: u8,
        compressed_text: Vec<u8>,
    },
    ITXT {
        keyword: String,
        compression_flag: u8,
        compression_method: u8,
        language_tag: String,
        translated_keyword: String,
        text: String,
    },
    BKGD(Vec<u8>),
    PHYS {
        ppux: u32,
        ppuy: u32,
        unit: u8,
    },
    TIME {
        year: u16,
        month: u8,
        day: u8,
        hour: u8,
        minute: u8,
        second: u8,
    },
    Unknown {
        chunk_type: [u8; 4],
        data: Vec<u8>,
    },
}

fn parse_png_signature(input: &[u8]) -> IResult<&[u8], [u8; 8], Error<&[u8]>> {
    let (input, signature) = take(8usize)(input)?;
    Ok((input, signature.try_into().unwrap()))
}

fn parse_chunk(input: &[u8]) -> IResult<&[u8], Chunk, Error<&[u8]>> {
    let (input, length) = be_u32(input)?;
    let (input, chunk_type) = take(4usize)(input)?;
    let (input, chunk_data) = take(length)(input)?;
    let (input, crc) = be_u32(input)?;

    let chunk_type_array: [u8; 4] = chunk_type.try_into().unwrap();
    let chunk_type_str = std::str::from_utf8(&chunk_type_array).unwrap();

    let data = match chunk_type_str {
        "IHDR" => parse_ihdr(chunk_data),
        "PLTE" => ChunkData::PLTE(parse_plte(chunk_data)),
        "IDAT" => ChunkData::IDAT(chunk_data.to_vec()),
        "IEND" => ChunkData::IEND,
        "tRNS" => ChunkData::TRNS(chunk_data.to_vec()),
        "cHRM" => parse_chrm(chunk_data),
        "gAMA" => parse_gama(chunk_data),
        "pHYs" => parse_phys(chunk_data),
        "tIME" => parse_time(chunk_data),
        _ => ChunkData::Unknown {
            chunk_type: chunk_type_array,
            data: chunk_data.to_vec(),
        },
    };

    Ok((input, Chunk {
        length,
        chunk_type: chunk_type_array,
        data,
        crc,
    }))
}

fn parse_ihdr(data: &[u8]) -> ChunkData {
    let (_, (width, height, bit_depth, color_type, compression_method, filter_method, interlace_method)) = 
        tuple((be_u32::<&[u8], Error<&[u8]>>, 
               be_u32::<&[u8], Error<&[u8]>>, 
               be_u8::<&[u8], Error<&[u8]>>, 
               be_u8::<&[u8], Error<&[u8]>>, 
               be_u8::<&[u8], Error<&[u8]>>, 
               be_u8::<&[u8], Error<&[u8]>>, 
               be_u8::<&[u8], Error<&[u8]>>))(data).unwrap();
    
    ChunkData::IHDR {
        width,
        height,
        bit_depth,
        color_type,
        compression_method,
        filter_method,
        interlace_method,
    }
}

fn parse_plte(data: &[u8]) -> Vec<(u8, u8, u8)> {
    data.chunks(3)
        .map(|chunk| (chunk[0], chunk[1], chunk[2]))
        .collect()
}

fn parse_chrm(data: &[u8]) -> ChunkData {
    let (_, (white_point_x, white_point_y, red_x, red_y, green_x, green_y, blue_x, blue_y)) = 
        tuple((be_u32::<&[u8], Error<&[u8]>>,
               be_u32::<&[u8], Error<&[u8]>>,
               be_u32::<&[u8], Error<&[u8]>>,
               be_u32::<&[u8], Error<&[u8]>>,
               be_u32::<&[u8], Error<&[u8]>>,
               be_u32::<&[u8], Error<&[u8]>>,
               be_u32::<&[u8], Error<&[u8]>>,
               be_u32::<&[u8], Error<&[u8]>>))(data).unwrap();
    
    ChunkData::CHRM {
        white_point_x,
        white_point_y,
        red_x,
        red_y,
        green_x,
        green_y,
        blue_x,
        blue_y,
    }
}

fn parse_gama(data: &[u8]) -> ChunkData {
    let (_, gamma) = be_u32::<&[u8], Error<&[u8]>>(data).unwrap();
    ChunkData::GAMA(gamma)
}

fn parse_phys(data: &[u8]) -> ChunkData {
    let (_, (ppux, ppuy, unit)) = tuple((
        be_u32::<&[u8], Error<&[u8]>>,
        be_u32::<&[u8], Error<&[u8]>>,
        be_u8::<&[u8], Error<&[u8]>>
    ))(data).unwrap();
    ChunkData::PHYS { ppux, ppuy, unit }
}

fn parse_time(data: &[u8]) -> ChunkData {
    let (_, (year, month, day, hour, minute, second)) = 
        tuple((be_u16::<&[u8], Error<&[u8]>>,
               be_u8::<&[u8], Error<&[u8]>>,
               be_u8::<&[u8], Error<&[u8]>>,
               be_u8::<&[u8], Error<&[u8]>>,
               be_u8::<&[u8], Error<&[u8]>>,
               be_u8::<&[u8], Error<&[u8]>>))(data).unwrap();
    
    ChunkData::TIME {
        year,
        month,
        day,
        hour,
        minute,
        second,
    }
}

fn parse_png(input: &[u8]) -> IResult<&[u8], PNG, Error<&[u8]>> {
    let (input, signature) = parse_png_signature(input)?;
    let (input, chunks) = many0(parse_chunk)(input)?;
    
    Ok((input, PNG { signature, chunks }))
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
        Err(e) => eprintln!("Failed to parse PNG: {:?}", e),
    }

    Ok(())
}