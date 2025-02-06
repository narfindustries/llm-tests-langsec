use nom::{
    bytes::complete::{tag, take},
    number::complete::be_u32,
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::path::Path;

#[derive(Debug)]
enum ChunkType {
    IHDR,
    PLTE,
    IDAT,
    IEND,
    CHRM,
    GAma,
    ICCP,
    SBIT,
    SRGB,
    BKGD,
    HIST,
    TRNS,
    PHys,
    TIME,
    TEXT,
    ZTXT,
    ITXT,
    OFFs,
}

impl From<&[u8; 4]> for ChunkType {
    fn from(bytes: &[u8; 4]) -> Self {
        match bytes {
            b"IHDR" => ChunkType::IHDR,
            b"PLTE" => ChunkType::PLTE,
            b"IDAT" => ChunkType::IDAT,
            b"IEND" => ChunkType::IEND,
            b"cHRM" => ChunkType::CHRM,
            b"gAMA" => ChunkType::GAma,
            b"iCCP" => ChunkType::ICCP,
            b"sBIT" => ChunkType::SBIT,
            b"sRGB" => ChunkType::SRGB,
            b"bKGD" => ChunkType::BKGD,
            b"hIST" => ChunkType::HIST,
            b"tRNS" => ChunkType::TRNS,
            b"pHYs" => ChunkType::PHys,
            b"tIME" => ChunkType::TIME,
            b"tEXt" => ChunkType::TEXT,
            b"zTXt" => ChunkType::ZTXT,
            b"iTXt" => ChunkType::ITXT,
            b"oFFs" => ChunkType::OFFs,
            _ => panic!("Unknown chunk type"),
        }
    }
}

fn chunk(input: &[u8]) -> IResult<&[u8], (ChunkType, &[u8])> {
    let (input, length) = be_u32(input)?;
    let (input, chunk_type_bytes) = take(4usize)(input)?;
    let chunk_type = ChunkType::from(
        &chunk_type_bytes
            .try_into()
            .expect("Failed to convert slice to array"),
    );
    let (input, data) = take(length as usize)(input)?;
    let (input, _) = take(4usize)(input)?; // CRC
    Ok((input, (chunk_type, data)))
}

fn png(input: &[u8]) -> IResult<&[u8], Vec<(ChunkType, &[u8])>> {
    let (input, _) = tag(b"\x89PNG\r\n\x1a\n")(input)?;
    let (input, chunks) = nom::multi::many0(chunk)(input)?;
    Ok((input, chunks))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <png_file>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let mut file = File::open(path).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match png(&buffer) {
        Ok((_, chunks)) => {
            println!("PNG Chunks:");
            for (chunk_type, data) in chunks {
                println!("  Type: {:?}, Data Length: {}", chunk_type, data.len());
            }
        }
        Err(e) => {
            println!("Error parsing PNG: {:?}", e);
        }
    }
}
