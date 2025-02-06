use nom::{
    bytes::complete::{tag, take, take_until},
    combinator::{map, map_res, opt},
    multi::{count, many0, tuple},
    number::complete::{be_u32, be_u8},
    sequence::tuple as nom_tuple,
    IResult,
};
use std::{fs::File, io::{Read, Write}, path::PathBuf};
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "png_parser", about = "Parse PNG files.")]
struct Opt {
    #[structopt(parse(from_os_str))]
    input_file: PathBuf,
}

#[derive(Debug)]
struct Png {
    header: IhdrChunk,
    palette: Option<PlteChunk>,
    data_chunks: Vec<IdatChunk>,
    transparency: Option<TrnsChunk>,
    background_color: Option<BkgdChunk>,
    gamma: Option<GammaChunk>,
    chromaticities: Option<ChrmChunk>,
    icc_profile: Option<IccpChunk>,
    significant_bits: Option<SbitChunk>,
    srgb: Option<SrgbChunk>,
    physical_dims: Option<PhysChunk>,
    last_chunk: IendChunk,
}

#[derive(Debug)]
struct IhdrChunk {
    width: u32,
    height: u32,
    bit_depth: u8,
    color_type: u8,
    compression_method: u8,
    filter_method: u8,
    interlace_method: u8,
}

#[derive(Debug)]
struct PlteChunk(Vec<(u8, u8, u8)>);

#[derive(Debug)]
struct IdatChunk(Vec<u8>);

#[derive(Debug)]
struct TrnsChunk(Vec<u8>);

#[derive(Debug)]
struct BkgdChunk(Vec<u8>);

#[derive(Debug)]
struct GammaChunk(u32);

#[derive(Debug)]
struct ChrmChunk {
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
struct IccpChunk {
    profile_name: String,
    compression_method: u8,
    compressed_profile: Vec<u8>,
}

#[derive(Debug)]
struct SbitChunk(Vec<u8>);

#[derive(Debug)]
struct SrgbChunk(u8);

#[derive(Debug)]
struct PhysChunk {
    pixels_per_unit_x: u32,
    pixels_per_unit_y: u32,
    unit: u8,
}

#[derive(Debug)]
struct IendChunk;

fn parse_png(input: &[u8]) -> IResult<&[u8], Png> {
    let (input, _) = tag(b"\x89PNG\r\n\x1a\n")(input)?;
    let (input, header) = parse_ihdr_chunk(input)?;
    let (input, palette) = opt(parse_plte_chunk)(input)?;
    let (input, data_chunks) = many0(parse_idat_chunk)(input)?;
    let (input, transparency) = opt(parse_trns_chunk)(input)?;
    let (input, background_color) = opt(parse_bkgd_chunk)(input)?;
    let (input, gamma) = opt(parse_gamma_chunk)(input)?;
    let (input, chromaticities) = opt(parse_chrm_chunk)(input)?;
    let (input, icc_profile) = opt(parse_iccp_chunk)(input)?;
    let (input, significant_bits) = opt(parse_sbit_chunk)(input)?;
    let (input, srgb) = opt(parse_srgb_chunk)(input)?;
    let (input, physical_dims) = opt(parse_phys_chunk)(input)?;
    let (input, last_chunk) = parse_iend_chunk(input)?;
    Ok((input, Png {
        header,
        palette,
        data_chunks,
        transparency,
        background_color,
        gamma,
        chromaticities,
        icc_profile,
        significant_bits,
        srgb,
        physical_dims,
        last_chunk,
    }))
}

fn parse_ihdr_chunk(input: &[u8]) -> IResult<&[u8], IhdrChunk> {
    let (input, _) = tag(b"IHDR")(input)?;
    let (input, (width, height, bit_depth, color_type, compression_method, filter_method, interlace_method)) =
        nom_tuple((be_u32, be_u32, be_u8, be_u8, be_u8, be_u8, be_u8))(input)?;
    Ok((input, IhdrChunk {
        width,
        height,
        bit_depth,
        color_type,
        compression_method,
        filter_method,
        interlace_method,
    }))
}

fn parse_plte_chunk(input: &[u8]) -> IResult<&[u8], PlteChunk> {
    let (input, _) = tag(b"PLTE")(input)?;
    let (input, entries) = count(nom_tuple((be_u8, be_u8, be_u8)), (input.len() / 3) as usize)(input)?;
    Ok((input, PlteChunk(entries)))
}

fn parse_idat_chunk(input: &[u8]) -> IResult<&[u8], IdatChunk> {
    let (input, _) = tag(b"IDAT")(input)?;
    let (input, data) = take(input.len())(input)?;
    Ok((input, IdatChunk(data.to_vec())))
}

fn parse_trns_chunk(input: &[u8]) -> IResult<&[u8], TrnsChunk> {
    let (input, _) = tag(b"tRNS")(input)?;
    let (input, data) = take(input.len())(input)?;
    Ok((input, TrnsChunk(data.to_vec())))
}

fn parse_bkgd_chunk(input: &[u8]) -> IResult<&[u8], BkgdChunk> {
    let (input, _) = tag(b"bKGD")(input)?;
    let (input, data) = take(input.len())(input)?;
    Ok((input, BkgdChunk(data.to_vec())))
}

fn parse_gamma_chunk(input: &[u8]) -> IResult<&[u8], GammaChunk> {
    let (input, _) = tag(b"gAMA")(input)?;
    let (input, gamma) = be_u32(input)?;
    Ok((input, GammaChunk(gamma)))
}

fn parse_chrm_chunk(input: &[u8]) -> IResult<&[u8], ChrmChunk> {
    let (input, _) = tag(b"cHRM")(input)?;
    let (input, (white_point_x, white_point_y, red_x, red_y, green_x, green_y, blue_x, blue_y)) =
        nom_tuple((be_u32, be_u32, be_u32, be_u32, be_u32, be_u32, be_u32, be_u32))(input)?;
    Ok((input, ChrmChunk {
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

fn parse_iccp_chunk(input: &[u8]) -> IResult<&[u8], IccpChunk> {
    let (input, _) = tag(b"iCCP")(input)?;
    let (input, profile_name) = map_res(take_until("\0"), std::str::from_utf8)(input)?;
    let (input, _) = take(1usize)(input)?;  // skip null byte
    let (input, compression_method) = be_u8(input)?;
    let (input, compressed_profile) = take(input.len())(input)?;
    Ok((input, IccpChunk {
        profile_name: profile_name.to_string(),
        compression_method,
        compressed_profile: compressed_profile.to_vec(),
    }))
}

fn parse_sbit_chunk(input: &[u8]) -> IResult<&[u8], SbitChunk> {
    let (input, _) = tag(b"sBIT")(input)?;
    let (input, data) = take(input.len())(input)?;
    Ok((input, SbitChunk(data.to_vec())))
}

fn parse_srgb_chunk(input: &[u8]) -> IResult<&[u8], SrgbChunk> {
    let (input, _) = tag(b"sRGB")(input)?;
    let (input, rendering_intent) = be_u8(input)?;
    Ok((input, SrgbChunk(rendering_intent)))
}

fn parse_phys_chunk(input: &[u8]) -> IResult<&[u8], PhysChunk> {
    let (input, _) = tag(b"pHYs")(input)?;
    let (input, (pixels_per_unit_x, pixels_per_unit_y, unit)) =
        nom_tuple((be_u32, be_u32, be_u8))(input)?;
    Ok((input, PhysChunk {
        pixels_per_unit_x,
        pixels_per_unit_y,
        unit,
    }))
}

fn parse_iend_chunk(input: &[u8]) -> IResult<&[u8], IendChunk> {
    let (input, _) = tag(b"IEND")(input)?;
    Ok((input, IendChunk))
}

fn main() -> io::Result<()> {
    let opts = Opt::from_args();
    let mut file = File::open(&opts.input_file)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    let result = parse_png(&buffer);
    match result {
        Ok((_, png)) => {
            println!("Parsed PNG: {:?}", png);
        },
        Err(e) => {
            eprintln!("Error parsing PNG: {:?}", e);
        }
    }

    Ok(())
}