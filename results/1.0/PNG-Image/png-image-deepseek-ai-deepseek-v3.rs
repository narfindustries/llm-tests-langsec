use nom::{
    bytes::complete::{take, take_until},
    combinator::map,
    number::complete::{be_u16, be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::fs;
use std::env;

#[derive(Debug)]
struct PngFile {
    signature: [u8; 8],
    chunks: Vec<Chunk>,
}

#[derive(Debug)]
enum Chunk {
    IHDR(IHDR),
    PLTE(Vec<RGB>),
    IDAT(Vec<u8>),
    IEND,
    tRNS(TRNS),
    bKGD(BKGD),
    cHRM(CHRM),
    gAMA(u32),
    hIST(Vec<u16>),
    iCCP(ICCP),
    pHYs(PHYs),
    sBIT(SBIT),
    sPLT(SPLT),
    sRGB(u8),
    tEXt(TEXT),
    zTXt(ZTXT),
    iTXt(ITXT),
    tIME(TIME),
}

#[derive(Debug)]
struct IHDR {
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
enum TRNS {
    Grayscale(u16),
    Truecolor(u16, u16, u16),
    Indexed(Vec<u8>),
}

#[derive(Debug)]
enum BKGD {
    Grayscale(u16),
    Truecolor(u16, u16, u16),
    Indexed(u8),
}

#[derive(Debug)]
struct CHRM {
    white_x: u32,
    white_y: u32,
    red_x: u32,
    red_y: u32,
    green_x: u32,
    green_y: u32,
    blue_x: u32,
    blue_y: u32,
}

#[derive(Debug)]
struct ICCP {
    profile_name: String,
    compression_method: u8,
    compressed_profile: Vec<u8>,
}

#[derive(Debug)]
struct PHYs {
    pixels_per_unit_x: u32,
    pixels_per_unit_y: u32,
    unit_specifier: u8,
}

#[derive(Debug)]
enum SBIT {
    Grayscale(u8),
    Truecolor(u8, u8, u8),
    Indexed(u8, u8, u8),
    GrayscaleAlpha(u8, u8),
    TruecolorAlpha(u8, u8, u8, u8),
}

#[derive(Debug)]
struct SPLT {
    palette_name: String,
    sample_depth: u8,
    entries: Vec<RGBAlpha>,
}

#[derive(Debug)]
struct RGBAlpha {
    r: u8,
    g: u8,
    b: u8,
    a: u8,
}

#[derive(Debug)]
struct TEXT {
    keyword: String,
    text: String,
}

#[derive(Debug)]
struct ZTXT {
    keyword: String,
    compression_method: u8,
    compressed_text: Vec<u8>,
}

#[derive(Debug)]
struct ITXT {
    keyword: String,
    compression_flag: u8,
    compression_method: u8,
    language_tag: String,
    translated_keyword: String,
    text: String,
}

#[derive(Debug)]
struct TIME {
    year: u16,
    month: u8,
    day: u8,
    hour: u8,
    minute: u8,
    second: u8,
}

fn parse_png(input: &[u8]) -> IResult<&[u8], PngFile> {
    let (input, signature) = take(8usize)(input)?;
    let (input, chunks) = nom::multi::many0(parse_chunk)(input)?;
    Ok((input, PngFile { signature: signature.try_into().unwrap(), chunks }))
}

fn parse_chunk(input: &[u8]) -> IResult<&[u8], Chunk> {
    let (input, length) = be_u32(input)?;
    let (input, chunk_type) = take(4usize)(input)?;
    let (input, chunk_data) = take(length)(input)?;
    let (input, crc) = take(4usize)(input)?;

    let chunk = match chunk_type {
        b"IHDR" => parse_ihdr(chunk_data).map(|(_, ihdr)| Chunk::IHDR(ihdr)),
        b"PLTE" => parse_plte(chunk_data).map(|(_, plte)| Chunk::PLTE(plte)),
        b"IDAT" => Ok(Chunk::IDAT(chunk_data.to_vec())),
        b"IEND" => Ok(Chunk::IEND),
        b"tRNS" => parse_trns(chunk_data).map(|(_, trns)| Chunk::tRNS(trns)),
        b"bKGD" => parse_bkgd(chunk_data).map(|(_, bkgd)| Chunk::bKGD(bkgd)),
        b"cHRM" => parse_chrm(chunk_data).map(|(_, chrm)| Chunk::cHRM(chrm)),
        b"gAMA" => parse_gama(chunk_data).map(|(_, gama)| Ch极速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速速进一步优化代码补全最后部分。```rust
fn parse_ihdr(input: &[u8]) -> IResult<&[u8], IHDR> {
    let (input, width) = be_u32(input)?;
    let (input, height) = be_u32(input)?;
    let (input, bit_depth) = be_u8(input)?;
    let (input, color_type) = be_u8(input)?;
    let (input, compression_method) = be_u8(input)?;
    let (input, filter_method) = be_u8(input)?;
    let (input, interlace_method) = be_u8(input)?;
    Ok((input, IHDR { width, height, bit_depth, color_type, compression_method, filter_method, interlace_method }))
}

fn parse_plte(input: &[u8]) -> IResult<&[u8], Vec<RGB>> {
    nom::multi::many0(parse_rgb)(input)
}

fn parse_rgb(input: &[u8]) -> IResult<&[u8], RGB> {
    let (input, r) = be_u8(input)?;
    let (input, g) = be_u8(input)?;
    let (input, b) = be_u8(input)?;
    Ok((input, RGB { r, g, b }))
}

fn parse_trns(input: &[u8]) -> IResult<&[u8], TRNS> {
    let (input, trns) = match input.len() {
        2 => map(be_u16, TRNS::Grayscale)(input),
        6 => map(tuple((be_u16, be_u16, be_u16)), |(r, g, b)| TRNS::Truecolor(r, g, b))(input),
        _ => map(take(input.len()), |data| TRNS::Indexed(data.to_vec()))(input),
    }?;
    Ok((input, trns))
}

fn parse_bkgd(input: &[u8]) -> IResult<&[u8], BKGD> {
    let (input, bkgd) = match input.len() {
        2 => map(be_u16, BKGD::Grayscale)(input),
        6 => map(tuple((be_u16, be_u