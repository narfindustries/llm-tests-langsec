use nom::{
    bytes::complete::{tag, take},
    combinator::{map_res, verify},
    error::{FromExternalError, ParseError, VerboseError},
    multi::many0,
    number::complete::{be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::{fs::File, io::{self, Read}, str::Utf8Error};

const PNG_SIGNATURE: &[u8] = &[137, 80, 78, 71, 13, 10, 26, 10];

#[derive(Debug)]
struct Chunk {
    length: u32,
    chunk_type: String,
    data: Vec<u8>,
    crc: u32,
}

#[derive(Debug)]
struct PNGImage {
    header: IHDRChunk,
    palette: Option<PLTEChunk>,
    data_chunks: Vec<IDATChunk>,
    other_chunks: Vec<Chunk>,
}

#[derive(Debug)]
struct IHDRChunk {
    width: u32,
    height: u32,
    bit_depth: u8,
    color_type: u8,
    compression_method: u8,
    filter_method: u8,
    interlace_method: u8,
}

#[derive(Debug)]
struct PLTEChunk {
    palette: Vec<(u8, u8, u8)>,
}

#[derive(Debug)]
struct IDATChunk {
    data: Vec<u8>,
}

fn parse_chunk<'a>(input: &'a [u8]) -> IResult<&'a [u8], Chunk, VerboseError<&'a [u8]>> {
    let (input, length) = be_u32(input)?;
    let (input, chunk_type) = map_res(take(4usize), std::str::from_utf8)(input)?;
    let (input, data) = take(length)(input)?;
    let (input, crc) = be_u32(input)?;
    Ok((
        input,
        Chunk {
            length,
            chunk_type: chunk_type.to_string(),
            data: Vec::from(data),
            crc,
        },
    ))
}

fn parse_png(input: &[u8]) -> IResult<&[u8], PNGImage, VerboseError<&[u8]>> {
    let (input, _) = tag(PNG_SIGNATURE)(input)?;

    let (input, ihdr_chunk) = verify(parse_chunk, |chunk: &Chunk| chunk.chunk_type == "IHDR")(input)?;
    let ihdr_data = parse_ihdr_chunk(&ihdr_chunk.data);

    let (input, chunks) = many0(parse_chunk)(input)?;
    let mut palette = None;
    let mut data_chunks = vec![];
    let mut other_chunks = vec![];

    for chunk in chunks {
        match chunk.chunk_type.as_str() {
            "PLTE" => palette = Some(parse_plte_chunk(&chunk.data)),
            "IDAT" => data_chunks.push(parse_idat_chunk(&chunk.data)),
            _ => other_chunks.push(chunk),
        }
    }

    Ok((
        input,
        PNGImage {
            header: ihdr_data,
            palette,
            data_chunks,
            other_chunks,
        },
    ))
}

fn parse_ihdr_chunk(input: &[u8]) -> IHDRChunk {
    let (_, (width, height, bit_depth, color_type, compression_method, filter_method, interlace_method)) =
        tuple((be_u32, be_u32, be_u8, be_u8, be_u8, be_u8, be_u8))(input).unwrap();
    IHDRChunk {
        width,
        height,
        bit_depth,
        color_type,
        compression_method,
        filter_method,
        interlace_method,
    }
}

fn parse_plte_chunk(input: &[u8]) -> PLTEChunk {
    let palette = input.chunks(3).map(|chunk| (chunk[0], chunk[1], chunk[2])).collect();
    PLTEChunk { palette }
}

fn parse_idat_chunk(input: &[u8]) -> IDATChunk {
    IDATChunk {
        data: Vec::from(input),
    }
}

fn main() -> io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut contents = Vec::new();
    file.read_to_end(&mut contents)?;

    match parse_png(&contents) {
        Ok((_, png)) => println!("{:#?}", png),
        Err(e) => println!("Error parsing PNG: {:?}", e),
    }

    Ok(())
}