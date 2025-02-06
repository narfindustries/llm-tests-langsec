use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u8, be_u16},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs;

fn parse_soi(input: &[u8]) -> IResult<&[u8], &[u8]> {
    tag(&[0xFF, 0xD8])(input)
}

fn parse_eoi(input: &[u8]) -> IResult<&[u8], &[u8]> {
    tag(&[0xFF, 0xD9])(input)
}

fn parse_app0(input: &[u8]) -> IResult<&[u8], (&[u8], u16, u8, u16, u16, u8, u8)> {
    let (input, _) = tag(&[0xFF, 0xE0])(input)?;
    let (input, length) = be_u16(input)?;
    let (input, identifier) = take(5u8)(input)?;
    let (input, version) = be_u16(input)?;
    let (input, density_units) = be_u8(input)?;
    let (input, x_density) = be_u16(input)?;
    let (input, y_density) = be_u16(input)?;
    let (input, thumb_width) = be_u8(input)?;
    let (input, thumb_height) = be_u8(input)?;
    Ok((
        input,
        (identifier, version, density_units, x_density, y_density, thumb_width, thumb_height),
    ))
}

fn parse_dqt(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, _) = tag(&[0xFF, 0xDB])(input)?;
    let (input, length) = be_u16(input)?;
    let (input, tables) = take(length - 2)(input)?;
    Ok((input, tables.to_vec()))
}

fn parse_sof0(input: &[u8]) -> IResult<&[u8], (u8, u16, u16, u8, Vec<(u8, u8, u8)>)> {
    let (input, _) = tag(&[0xFF, 0xC0])(input)?;
    let (input, length) = be_u16(input)?;
    let (input, precision) = be_u8(input)?;
    let (input, height) = be_u16(input)?;
    let (input, width) = be_u16(input)?;
    let (input, components) = be_u8(input)?;
    let (input, component_params) = take(components as usize * 3)(input)?;
    let component_params = component_params
        .chunks(3)
        .map(|chunk| (chunk[0], chunk[1], chunk[2]))
        .collect();
    Ok((input, (precision, height, width, components, component_params)))
}

fn parse_dht(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, _) = tag(&[0xFF, 0xC4])(input)?;
    let (input, length) = be_u16(input)?;
    let (input, tables) = take(length - 2)(input)?;
    Ok((input, tables.to_vec()))
}

fn parse_sos(input: &[u8]) -> IResult<&[u8], (u8, Vec<(u8, u8)>, u8, u8, u8)> {
    let (input, _) = tag(&[0xFF, 0xDA])(input)?;
    let (input, length) = be_u16(input)?;
    let (input, components) = be_u8(input)?;
    let (input, component_params) = take(components as usize * 2)(input)?;
    let component_params = component_params
        .chunks(2)
        .map(|chunk| (chunk[0], chunk[1]))
        .collect();
    let (input, spectral_start) = be_u8(input)?;
    let (input, spectral_end) = be_u8(input)?;
    let (input, successive_approx) = be_u8(input)?;
    Ok((input, (components, component_params, spectral_start, spectral_end, successive_approx)))
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = parse_soi(input)?;
    let (input, _app0) = parse_app0(input)?;
    let (input, _dqt) = parse_dqt(input)?;
    let (input, _sof0) = parse_sof0(input)?;
    let (input, _dht) = parse_dht(input)?;
    let (input, _sos) = parse_sos(input)?;
    let (input, _) = parse_eoi(input)?;
    Ok((input, ()))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return;
    }
    let file_path = &args[1];
    let data = fs::read(file_path).expect("Failed to read file");
    match parse_jpeg(&data) {
        Ok((_remaining, _)) => println!("JPEG parsed successfully"),
        Err(e) => eprintln!("Failed to parse JPEG: {:?}", e),
    }
}