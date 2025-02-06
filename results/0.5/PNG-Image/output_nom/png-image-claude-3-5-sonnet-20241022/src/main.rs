use nom::{
    bytes::complete::{tag, take},
    combinator::{map, verify},
    multi::many0,
    number::complete::{be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::{env, fs, path::Path};

const PNG_SIGNATURE: &[u8] = &[0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A];

#[derive(Debug)]
struct PNG {
    header: IHDR,
    palette: Option<PLTE>,
    transparency: Option<tRNS>,
    chromaticity: Option<cHRM>,
    gamma: Option<gAMA>,
    icc_profile: Option<iCCP>,
    significant_bits: Option<sBIT>,
    srgb: Option<sRGB>,
    text_chunks: Vec<TextChunk>,
    background: Option<bKGD>,
    physical_dims: Option<pHYs>,
    time: Option<tIME>,
    image_data: Vec<IDAT>,
}

#[derive(Debug)]
struct IHDR {
    width: u32,
    height: u32,
    bit_depth: u8,
    color_type: u8,
    compression: u8,
    filter: u8,
    interlace: u8,
}

#[derive(Debug)]
struct PLTE(Vec<(u8, u8, u8)>);

#[derive(Debug)]
struct IDAT(Vec<u8>);

#[derive(Debug)]
struct tRNS(Vec<u8>);

#[derive(Debug)]
struct cHRM {
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
struct gAMA(u32);

#[derive(Debug)]
struct iCCP {
    name: String,
    profile: Vec<u8>,
}

#[derive(Debug)]
struct sBIT(Vec<u8>);

#[derive(Debug)]
struct sRGB(u8);

#[derive(Debug)]
enum TextChunk {
    Text { keyword: String, text: String },
    CompressedText { keyword: String, text: Vec<u8> },
    InternationalText {
        keyword: String,
        language: String,
        translated_keyword: String,
        text: String,
    },
}

#[derive(Debug)]
struct bKGD(Vec<u8>);

#[derive(Debug)]
struct pHYs {
    x: u32,
    y: u32,
    unit: u8,
}

#[derive(Debug)]
struct tIME {
    year: u16,
    month: u8,
    day: u8,
    hour: u8,
    minute: u8,
    second: u8,
}

fn parse_png(input: &[u8]) -> IResult<&[u8], PNG> {
    let (input, _) = tag(PNG_SIGNATURE)(input)?;
    let (input, chunks) = many0(parse_chunk)(input)?;

    let mut png = PNG {
        header: IHDR {
            width: 0,
            height: 0,
            bit_depth: 0,
            color_type: 0,
            compression: 0,
            filter: 0,
            interlace: 0,
        },
        palette: None,
        transparency: None,
        chromaticity: None,
        gamma: None,
        icc_profile: None,
        significant_bits: None,
        srgb: None,
        text_chunks: Vec::new(),
        background: None,
        physical_dims: None,
        time: None,
        image_data: Vec::new(),
    };

    for chunk in chunks {
        match chunk {
            Chunk::IHDR(ihdr) => png.header = ihdr,
            Chunk::PLTE(plte) => png.palette = Some(plte),
            Chunk::IDAT(idat) => png.image_data.push(idat),
            Chunk::tRNS(trns) => png.transparency = Some(trns),
            Chunk::cHRM(chrm) => png.chromaticity = Some(chrm),
            Chunk::gAMA(gama) => png.gamma = Some(gama),
            Chunk::iCCP(iccp) => png.icc_profile = Some(iccp),
            Chunk::sBIT(sbit) => png.significant_bits = Some(sbit),
            Chunk::sRGB(srgb) => png.srgb = Some(srgb),
            Chunk::tEXt(text) | Chunk::zTXt(text) | Chunk::iTXt(text) => png.text_chunks.push(text),
            Chunk::bKGD(bkgd) => png.background = Some(bkgd),
            Chunk::pHYs(phys) => png.physical_dims = Some(phys),
            Chunk::tIME(time) => png.time = Some(time),
            Chunk::IEND => break,
        }
    }

    Ok((input, png))
}

#[derive(Debug)]
enum Chunk {
    IHDR(IHDR),
    PLTE(PLTE),
    IDAT(IDAT),
    IEND,
    tRNS(tRNS),
    cHRM(cHRM),
    gAMA(gAMA),
    iCCP(iCCP),
    sBIT(sBIT),
    sRGB(sRGB),
    tEXt(TextChunk),
    zTXt(TextChunk),
    iTXt(TextChunk),
    bKGD(bKGD),
    pHYs(pHYs),
    tIME(tIME),
}

fn parse_chunk(input: &[u8]) -> IResult<&[u8], Chunk> {
    let (input, length) = be_u32(input)?;
    let (input, chunk_type) = take(4usize)(input)?;
    let (input, chunk_data) = take(length)(input)?;
    let (input, _crc) = take(4usize)(input)?;

    let chunk = match chunk_type {
        b"IHDR" => Chunk::IHDR(parse_ihdr(chunk_data)?.1),
        b"PLTE" => Chunk::PLTE(PLTE(chunk_data.chunks(3).map(|rgb| (rgb[0], rgb[1], rgb[2])).collect())),
        b"IDAT" => Chunk::IDAT(IDAT(chunk_data.to_vec())),
        b"IEND" => Chunk::IEND,
        b"tRNS" => Chunk::tRNS(tRNS(chunk_data.to_vec())),
        b"cHRM" => Chunk::cHRM(parse_chrm(chunk_data)?.1),
        b"gAMA" => Chunk::gAMA(gAMA(be_u32(chunk_data)?.1)),
        b"iCCP" => Chunk::iCCP(parse_iccp(chunk_data)?.1),
        b"sBIT" => Chunk::sBIT(sBIT(chunk_data.to_vec())),
        b"sRGB" => Chunk::sRGB(sRGB(chunk_data[0])),
        b"tEXt" => Chunk::tEXt(parse_text(chunk_data)?.1),
        b"zTXt" => Chunk::zTXt(parse_compressed_text(chunk_data)?.1),
        b"iTXt" => Chunk::iTXt(parse_international_text(chunk_data)?.1),
        b"bKGD" => Chunk::bKGD(bKGD(chunk_data.to_vec())),
        b"pHYs" => Chunk::pHYs(parse_phys(chunk_data)?.1),
        b"tIME" => Chunk::tIME(parse_time(chunk_data)?.1),
        _ => return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
    };

    Ok((input, chunk))
}

fn parse_ihdr(input: &[u8]) -> IResult<&[u8], IHDR> {
    let (input, (width, height, bit_depth, color_type, compression, filter, interlace)) = tuple((
        be_u32,
        be_u32,
        be_u8,
        be_u8,
        be_u8,
        be_u8,
        be_u8,
    ))(input)?;

    Ok((
        input,
        IHDR {
            width,
            height,
            bit_depth,
            color_type,
            compression,
            filter,
            interlace,
        },
    ))
}

fn parse_chrm(input: &[u8]) -> IResult<&[u8], cHRM> {
    let (input, (white_x, white_y, red_x, red_y, green_x, green_y, blue_x, blue_y)) = tuple((
        be_u32, be_u32, be_u32, be_u32, be_u32, be_u32, be_u32, be_u32,
    ))(input)?;

    Ok((
        input,
        cHRM {
            white_x,
            white_y,
            red_x,
            red_y,
            green_x,
            green_y,
            blue_x,
            blue_y,
        },
    ))
}

fn parse_iccp(input: &[u8]) -> IResult<&[u8], iCCP> {
    let (input, name_bytes) = take_until_null(input)?;
    let (input, _) = tag(&[0])(input)?;
    let (input, _compression_method) = verify(be_u8, |&x| x == 0)(input)?;
    let name = String::from_utf8_lossy(name_bytes).into_owned();
    let profile = input.to_vec();

    Ok((&[], iCCP { name, profile }))
}

fn parse_text(input: &[u8]) -> IResult<&[u8], TextChunk> {
    let (input, keyword_bytes) = take_until_null(input)?;
    let (input, _) = tag(&[0])(input)?;
    let keyword = String::from_utf8_lossy(keyword_bytes).into_owned();
    let text = String::from_utf8_lossy(input).into_owned();

    Ok((&[], TextChunk::Text { keyword, text }))
}

fn parse_compressed_text(input: &[u8]) -> IResult<&[u8], TextChunk> {
    let (input, keyword_bytes) = take_until_null(input)?;
    let (input, _) = tag(&[0])(input)?;
    let (input, _compression_method) = verify(be_u8, |&x| x == 0)(input)?;
    let keyword = String::from_utf8_lossy(keyword_bytes).into_owned();
    let text = input.to_vec();

    Ok((
        &[],
        TextChunk::CompressedText { keyword, text },
    ))
}

fn parse_international_text(input: &[u8]) -> IResult<&[u8], TextChunk> {
    let (input, keyword_bytes) = take_until_null(input)?;
    let (input, _) = tag(&[0])(input)?;
    let (input, _compression_flag) = be_u8(input)?;
    let (input, _compression_method) = be_u8(input)?;
    let (input, language_bytes) = take_until_null(input)?;
    let (input, _) = tag(&[0])(input)?;
    let (input, translated_keyword_bytes) = take_until_null(input)?;
    let (input, _) = tag(&[0])(input)?;

    let keyword = String::from_utf8_lossy(keyword_bytes).into_owned();
    let language = String::from_utf8_lossy(language_bytes).into_owned();
    let translated_keyword = String::from_utf8_lossy(translated_keyword_bytes).into_owned();
    let text = String::from_utf8_lossy(input).into_owned();

    Ok((
        &[],
        TextChunk::InternationalText {
            keyword,
            language,
            translated_keyword,
            text,
        },
    ))
}

fn parse_phys(input: &[u8]) -> IResult<&[u8], pHYs> {
    let (input, (x, y, unit)) = tuple((be_u32, be_u32, be_u8))(input)?;
    Ok((input, pHYs { x, y, unit }))
}

fn parse_time(input: &[u8]) -> IResult<&[u8], tIME> {
    let (input, (year, month, day, hour, minute, second)) = tuple((
        map(take(2usize), |b: &[u8]| ((b[0] as u16) << 8) | b[1] as u16),
        be_u8,
        be_u8,
        be_u8,
        be_u8,
        be_u8,
    ))(input)?;

    Ok((
        input,
        tIME {
            year,
            month,
            day,
            hour,
            minute,
            second,
        },
    ))
}

fn take_until_null(input: &[u8]) -> IResult<&[u8], &[u8]> {
    let mut i = 0;
    while i < input.len() && input[i] != 0 {
        i += 1;
    }
    Ok((&input[i..], &input[0..i]))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <png-file>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let data = fs::read(path).expect("Failed to read file");
    
    match parse_png(&data) {
        Ok((_, png)) => println!("{:#?}", png),
        Err(e) => eprintln!("Failed to parse PNG: {:?}", e),
    }
}