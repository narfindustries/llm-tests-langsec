use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    error::{Error, ErrorKind},
    multi::{many0, many1},
    number::complete::{be_u16, be_u32, be_u8},
    sequence::{delimited, tuple},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
};

#[derive(Debug, PartialEq)]
enum ColorType {
    Grayscale,
    RGB,
    Palette,
    GreyscaleAlpha,
    RGBA,
}

#[derive(Debug, PartialEq)]
enum InterlaceMethod {
    NoInterlace,
    Adam7,
}

#[derive(Debug, PartialEq)]
enum CompressionMethod {
    DeflateInflate,
}

#[derive(Debug, PartialEq)]
enum FilterMethod {
    AdaptiveFiltering,
}

#[derive(Debug, PartialEq)]
struct IHDR {
    width: u32,
    height: u32,
    bit_depth: u8,
    color_type: ColorType,
    compression_method: CompressionMethod,
    filter_method: FilterMethod,
    interlace_method: InterlaceMethod,
}

#[derive(Debug, PartialEq)]
struct PLTE {
    palette_entries: Vec<(u8, u8, u8)>,
}

#[derive(Debug, PartialEq)]
struct IDAT {
    compressed_data: Vec<u8>,
}

#[derive(Debug, PartialEq)]
struct IEND {}

#[derive(Debug, PartialEq)]
struct tRNS {
    transparency_values: Vec<u8>,
}

#[derive(Debug, PartialEq)]
struct cHRM {
    white_point_x: u32,
    white_point_y: u32,
    red_x: u32,
    red_y: u32,
    green_x: u32,
    green_y: u32,
    blue_x: u32,
    blue_y: u32,
}

#[derive(Debug, PartialEq)]
struct gAMA {
    gamma_value: u32,
}

#[derive(Debug, PartialEq)]
struct iCCP {
    profile_name: String,
    compression_method: CompressionMethod,
    compressed_profile: Vec<u8>,
}

#[derive(Debug, PartialEq)]
struct sBIT {
    significant_bits: (u8, u8, u8),
}

#[derive(Debug, PartialEq)]
struct sRGB {
    rendering_intent: u8,
}

#[derive(Debug, PartialEq)]
struct tXYZ {
    transformation_matrix: [(i32, i32, i32); 3],
}

#[derive(Debug, PartialEq)]
struct bKGD {
    background_color: (u8, u8, u8),
}

#[derive(Debug, PartialEq)]
struct hIST {
    histogram_entries: Vec<u16>,
}

#[derive(Debug, PartialEq)]
struct pHYs {
    pixels_per_unit_x: u32,
    pixels_per_unit_y: u32,
    unit_specifier: u8,
}

#[derive(Debug, PartialEq)]
struct sCAL {
    unit_specifier: u8,
    width: u32,
    height: u32,
}

#[derive(Debug, PartialEq)]
struct tEXt {
    keyword: String,
    text_data: String,
}

#[derive(Debug, PartialEq)]
struct zTXt {
    keyword: String,
    compression_method: CompressionMethod,
    compressed_text_data: Vec<u8>,
}

#[derive(Debug, PartialEq)]
struct iTXt {
    keyword: String,
    compression_flag: bool,
    compression_method: CompressionMethod,
    language_tag: String,
    translated_keyword: String,
    text_data: String,
}

#[derive(Debug, PartialEq)]
struct fRAc {
    numerator: u32,
    denominator: u32,
}

fn magic_number(input: &[u8]) -> IResult<&[u8], ()> {
    tag(&[0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a])(input).map(|(input, _)| (input, ()))
}

fn ihdr(input: &[u8]) -> IResult<&[u8], IHDR> {
    let (input, _) = tag("IHDR")(input)?;
    let (input, width) = be_u32(input)?;
    let (input, height) = be_u32(input)?;
    let (input, bit_depth) = be_u8(input)?;
    let (input, color_type) = map(be_u8, |color_type| match color_type {
        0 => ColorType::Grayscale,
        2 => ColorType::RGB,
        3 => ColorType::Palette,
        4 => ColorType::GreyscaleAlpha,
        6 => ColorType::RGBA,
        _ => panic!("Invalid color type"),
    })(input)?;
    let (input, _) = be_u8(input)?; // compression method
    let (input, _) = be_u8(input)?; // filter method
    let (input, interlace_method) = map(be_u8, |interlace_method| match interlace_method {
        0 => InterlaceMethod::NoInterlace,
        1 => InterlaceMethod::Adam7,
        _ => panic!("Invalid interlace method"),
    })(input)?;
    Ok((input, IHDR {
        width,
        height,
        bit_depth,
        color_type,
        compression_method: CompressionMethod::DeflateInflate,
        filter_method: FilterMethod::AdaptiveFiltering,
        interlace_method,
    }))
}

fn plte(input: &[u8]) -> IResult<&[u8], PLTE> {
    let (input, _) = tag("PLTE")(input)?;
    let (input, palette_entries) = many1(tuple((be_u8, be_u8, be_u8)))(input)?;
    Ok((input, PLTE { palette_entries }))
}

fn idat(input: &[u8]) -> IResult<&[u8], IDAT> {
    let (input, _) = tag("IDAT")(input)?;
    let (input, compressed_data) = take(4)(input)?;
    Ok((input, IDAT { compressed_data: compressed_data.to_vec() }))
}

fn iend(input: &[u8]) -> IResult<&[u8], IEND> {
    let (input, _) = tag("IEND")(input)?;
    Ok((input, IEND {}))
}

fn trns(input: &[u8]) -> IResult<&[u8], tRNS> {
    let (input, _) = tag("tRNS")(input)?;
    let (input, transparency_values) = many1(be_u8)(input)?;
    Ok((input, tRNS { transparency_values }))
}

fn chrm(input: &[u8]) -> IResult<&[u8], cHRM> {
    let (input, _) = tag("cHRM")(input)?;
    let (input, white_point_x) = be_u32(input)?;
    let (input, white_point_y) = be_u32(input)?;
    let (input, red_x) = be_u32(input)?;
    let (input, red_y) = be_u32(input)?;
    let (input, green_x) = be_u32(input)?;
    let (input, green_y) = be_u32(input)?;
    let (input, blue_x) = be_u32(input)?;
    let (input, blue_y) = be_u32(input)?;
    Ok((input, cHRM {
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

fn gama(input: &[u8]) -> IResult<&[u8], gAMA> {
    let (input, _) = tag("gAMA")(input)?;
    let (input, gamma_value) = be_u32(input)?;
    Ok((input, gAMA { gamma_value }))
}

fn iccp(input: &[u8]) -> IResult<&[u8], iCCP> {
    let (input, _) = tag("iCCP")(input)?;
    let (input, profile_name) = map_res(take(79), |profile_name| String::from_utf8_lossy(profile_name).into_owned())(input)?;
    let (input, _) = be_u8(input)?; // compression method
    let (input, compressed_profile) = take(4)(input)?;
    Ok((input, iCCP {
        profile_name,
        compression_method: CompressionMethod::DeflateInflate,
        compressed_profile: compressed_profile.to_vec(),
    }))
}

fn sbit(input: &[u8]) -> IResult<&[u8], sBIT> {
    let (input, _) = tag("sBIT")(input)?;
    let (input, significant_bits) = tuple((be_u8, be_u8, be_u8))(input)?;
    Ok((input, sBIT { significant_bits }))
}

fn srgb(input: &[u8]) -> IResult<&[u8], sRGB> {
    let (input, _) = tag("sRGB")(input)?;
    let (input, rendering_intent) = be_u8(input)?;
    Ok((input, sRGB { rendering_intent }))
}

fn txyz(input: &[u8]) -> IResult<&[u8], tXYZ> {
    let (input, _) = tag("tXYZ")(input)?;
    let (input, transformation_matrix) = many1(tuple((be_u32, be_u32, be_u32)))(input)?;
    Ok((input, tXYZ {
        transformation_matrix: transformation_matrix
            .into_iter()
            .map(|(a, b, c)| (a as i32, b as i32, c as i32))
            .collect::<Vec<_>>()
            .try_into()
            .unwrap(),
    }))
}

fn bkgd(input: &[u8]) -> IResult<&[u8], bKGD> {
    let (input, _) = tag("bKGD")(input)?;
    let (input, background_color) = tuple((be_u8, be_u8, be_u8))(input)?;
    Ok((input, bKGD { background_color }))
}

fn hist(input: &[u8]) -> IResult<&[u8], hIST> {
    let (input, _) = tag("hIST")(input)?;
    let (input, histogram_entries) = many1(be_u16)(input)?;
    Ok((input, hIST { histogram_entries }))
}

fn phys(input: &[u8]) -> IResult<&[u8], pHYs> {
    let (input, _) = tag("pHYs")(input)?;
    let (input, pixels_per_unit_x) = be_u32(input)?;
    let (input, pixels_per_unit_y) = be_u32(input)?;
    let (input, unit_specifier) = be_u8(input)?;
    Ok((input, pHYs {
        pixels_per_unit_x,
        pixels_per_unit_y,
        unit_specifier,
    }))
}

fn scal(input: &[u8]) -> IResult<&[u8], sCAL> {
    let (input, _) = tag("sCAL")(input)?;
    let (input, unit_specifier) = be_u8(input)?;
    let (input, width) = be_u32(input)?;
    let (input, height) = be_u32(input)?;
    Ok((input, sCAL { unit_specifier, width, height }))
}

fn tuxt(input: &[u8]) -> IResult<&[u8], tEXt> {
    let (input, _) = tag("tEXt")(input)?;
    let (input, keyword) = map_res(take(79), |keyword| String::from_utf8_lossy(keyword).into_owned())(input)?;
    let (input, text_data) = map_res(take(79), |text_data| String::from_utf8_lossy(text_data).into_owned())(input)?;
    Ok((input, tEXt { keyword, text_data }))
}

fn ztzt(input: &[u8]) -> IResult<&[u8], zTXt> {
    let (input, _) = tag("zTXt")(input)?;
    let (input, keyword) = map_res(take(79), |keyword| String::from_utf8_lossy(keyword).into_owned())(input)?;
    let (input, _) = be_u8(input)?; // compression method
    let (input, compressed_text_data) = take(4)(input)?;
    Ok((input, zTXt {
        keyword,
        compression_method: CompressionMethod::DeflateInflate,
        compressed_text_data: compressed_text_data.to_vec(),
    }))
}

fn itxt(input: &[u8]) -> IResult<&[u8], iTXt> {
    let (input, _) = tag("iTXt")(input)?;
    let (input, keyword) = map_res(take(79), |keyword| String::from_utf8_lossy(keyword).into_owned())(input)?;
    let (input, compression_flag) = map(be_u8, |compression_flag| compression_flag != 0)(input)?;
    let (input, _) = be_u8(input)?; // compression method
    let (input, language_tag) = map_res(take(79), |language_tag| String::from_utf8_lossy(language_tag).into_owned())(input)?;
    let (input, translated_keyword) = map_res(take(79), |translated_keyword| String::from_utf8_lossy(translated_keyword).into_owned())(input)?;
    let (input, text_data) = map_res(take(79), |text_data| String::from_utf8_lossy(text_data).into_owned())(input)?;
    Ok((input, iTXt {
        keyword,
        compression_flag,
        compression_method: CompressionMethod::DeflateInflate,
        language_tag,
        translated_keyword,
        text_data,
    }))
}

fn frac(input: &[u8]) -> IResult<&[u8], fRAc> {
    let (input, _) = tag("fRAc")(input)?;
    let (input, numerator) = be_u32(input)?;
    let (input, denominator) = be_u32(input)?;
    Ok((input, fRAc { numerator, denominator }))
}

fn png(input: &[u8]) -> IResult<&[u8], Vec<Box<dyn std::any::Any>>> {
    let (input, _) = magic_number(input)?;
    let mut chunks = Vec::new();
    loop {
        let (input, chunk_type) = take(4u8)(input)?;
        match chunk_type {
            b"IHDR" => {
                let (input, ihdr) = ihdr(input)?;
                chunks.push(Box::new(ihdr));
            }
            b"PLTE" => {
                let (input, plte) = plte(input)?;
                chunks.push(Box::new(plte));
            }
            b"IDAT" => {
                let (input, idat) = idat(input)?;
                chunks.push(Box::new(idat));
            }
            b"IEND" => {
                let (input, _) = iend(input)?;
                break;
            }
            b"tRNS" => {
                let (input, trns) = trns(input)?;
                chunks.push(Box::new(trns));
            }
            b"cHRM" => {
                let (input, chrm) = chrm(input)?;
                chunks.push(Box::new(chrm));
            }
            b"gAMA" => {
                let (input, gama) = gama(input)?;
                chunks.push(Box::new(gama));
            }
            b"iCCP" => {
                let (input, iccp) = iccp(input)?;
                chunks.push(Box::new(iccp));
            }
            b"sBIT" => {
                let (input, sbit) = sbit(input)?;
                chunks.push(Box::new(sbit));
            }
            b"sRGB" => {
                let (input, srgb) = srgb(input)?;
                chunks.push(Box::new(srgb));
            }
            b"tXYZ" => {
                let (input, txyz) = txyz(input)?;
                chunks.push(Box::new(txyz));
            }
            b"bKGD" => {
                let (input, bkgd) = bkgd(input)?;
                chunks.push(Box::new(bkgd));
            }
            b"hIST" => {
                let (input, hist) = hist(input)?;
                chunks.push(Box::new(hist));
            }
            b"pHYs" => {
                let (input, phys) = phys(input)?;
                chunks.push(Box::new(phys));
            }
            b"sCAL" => {
                let (input, scal) = scal(input)?;
                chunks.push(Box::new(scal));
            }
            b"tEXt" => {
                let (input, tuxt) = tuxt(input)?;
                chunks.push(Box::new(tuxt));
            }
            b"zTXt" => {
                let (input, ztzt) = ztzt(input)?;
                chunks.push(Box::new(ztzt));
            }
            b"iTXt" => {
                let (input, itxt) = itxt(input)?;
                chunks.push(Box::new(itxt));
            }
            b"fRAc" => {
                let (input, frac) = frac(input)?;
                chunks.push(Box::new(frac));
            }
            _ => {
                let (input, _) = take(4)(input)?;
                continue;
            }
        }
    }
    Ok((input, chunks))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <input_file>", args[0]);
    }
    let file = File::open(&args[1]).unwrap();
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input).unwrap();
    let (_rest, chunks) = png(&input).unwrap();
    for chunk in chunks {
        match chunk.downcast_ref::<IHDR>() {
            Some(ihdr) => println!("IHDR: {:?}", ihdr),
            None => (),
        }
        match chunk.downcast_ref::<PLTE>() {
            Some(plte) => println!("PLTE: {:?}", plte),
            None => (),
        }
        match chunk.downcast_ref::<IDAT>() {
            Some(idat) => println!("IDAT: {:?}", idat),
            None => (),
        }
        match chunk.downcast_ref::<IEND>() {
            Some(_) => println!("IEND"),
            None => (),
        }
        match chunk.downcast_ref::<tRNS>() {
            Some(trns) => println!("tRNS: {:?}", trns),
            None => (),
        }
        match chunk.downcast_ref::<cHRM>() {
            Some(chrm) => println!("cHRM: {:?}", chrm),
            None => (),
        }
        match chunk.downcast_ref::<gAMA>() {
            Some(gama) => println!("gAMA: {:?}", gama),
            None => (),
        }
        match chunk.downcast_ref::<iCCP>() {
            Some(iccp) => println!("iCCP: {:?}", iccp),
            None => (),
        }
        match chunk.downcast_ref::<sBIT>() {
            Some(sbit) => println!("sBIT: {:?}", sbit),
            None => (),
