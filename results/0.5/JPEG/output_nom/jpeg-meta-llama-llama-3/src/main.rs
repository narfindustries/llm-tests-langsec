use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{many0, many1},
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::{
    fs::File,
    io::{BufReader, Read},
    path::Path,
};

#[derive(Debug)]
struct JpegFile {
    soi: u8,
    app0: App0,
    app1: Option<App1>,
    other_segments: Vec<OtherSegment>,
    sos: SOS,
    image_data: Vec<u8>,
    eoi: u8,
}

#[derive(Debug)]
struct App0 {
    marker: u8,
    length: u16,
    identifier: [u8; 5],
    version: [u8; 2],
    units: u8,
    x_density: u16,
    y_density: u16,
    thumbnail_x: u8,
    thumbnail_y: u8,
}

#[derive(Debug)]
struct App1 {
    marker: u8,
    length: u16,
    identifier: [u8; 5],
    exif_data: Vec<u8>,
}

#[derive(Debug)]
struct OtherSegment {
    marker: u8,
    length: u16,
    data: Vec<u8>,
}

#[derive(Debug)]
struct SOS {
    marker: u8,
    length: u16,
    num_components: u8,
    components: Vec<Component>,
    start_spectral_selection: u8,
    end_spectral_selection: u8,
    successive_approximation: u8,
}

#[derive(Debug)]
struct Component {
    identifier: u8,
    sampling_factor: u8,
    quantization_table: u8,
}

fn parse_jpeg_file(input: &[u8]) -> IResult<&[u8], JpegFile> {
    let (input, soi) = tag(&[0xff, 0xd8])(input)?;
    let (input, app0) = parse_app0(input)?;
    let (input, app1) = opt(parse_app1)(input)?;
    let (input, other_segments) = many0(parse_other_segment)(input)?;
    let (input, sos) = parse_sos(input)?;
    let (input, image_data) = take(take!(input, &mut |i| i.len() - 2))(input)?;
    let (input, eoi) = tag(&[0xff, 0xd9])(input)?;
    Ok((
        input,
        JpegFile {
            soi: soi[0],
            app0,
            app1,
            other_segments,
            sos,
            image_data: image_data.to_vec(),
            eoi: eoi[0],
        },
    ))
}

fn parse_app0(input: &[u8]) -> IResult<&[u8], App0> {
    let (input, marker) = tag(&[0xff, 0xe0])(input)?;
    let (input, length) = be_u16(input)?;
    let (input, identifier) = take(5u8)(input)?;
    let (input, version) = take(2u8)(input)?;
    let (input, units) = be_u8(input)?;
    let (input, x_density) = be_u16(input)?;
    let (input, y_density) = be_u16(input)?;
    let (input, thumbnail_x) = be_u8(input)?;
    let (input, thumbnail_y) = be_u8(input)?;
    let app0 = App0 {
        marker: marker[0],
        length,
        identifier: identifier.try_into().unwrap(),
        version: version.try_into().unwrap(),
        units,
        x_density,
        y_density,
        thumbnail_x,
        thumbnail_y,
    };
    Ok((input, app0))
}

fn parse_app1(input: &[u8]) -> IResult<&[u8], App1> {
    let (input, marker) = tag(&[0xff, 0xe1])(input)?;
    let (input, length) = be_u16(input)?;
    let (input, identifier) = take(5u8)(input)?;
    let (input, exif_data) = take(length - 8)(input)?;
    let app1 = App1 {
        marker: marker[0],
        length,
        identifier: identifier.try_into().unwrap(),
        exif_data: exif_data.to_vec(),
    };
    Ok((input, app1))
}

fn parse_other_segment(input: &[u8]) -> IResult<&[u8], OtherSegment> {
    let (input, marker) = be_u8(input)?;
    let (input, length) = be_u16(input)?;
    let (input, data) = take(length - 2)(input)?;
    let other_segment = OtherSegment {
        marker,
        length,
        data: data.to_vec(),
    };
    Ok((input, other_segment))
}

fn parse_sos(input: &[u8]) -> IResult<&[u8], SOS> {
    let (input, marker) = tag(&[0xff, 0xda])(input)?;
    let (input, length) = be_u16(input)?;
    let (input, num_components) = be_u8(input)?;
    let (input, components) = many1(parse_component)(input)?;
    let (input, start_spectral_selection) = be_u8(input)?;
    let (input, end_spectral_selection) = be_u8(input)?;
    let (input, successive_approximation) = be_u8(input)?;
    let sos = SOS {
        marker: marker[0],
        length,
        num_components,
        components,
        start_spectral_selection,
        end_spectral_selection,
        successive_approximation,
    };
    Ok((input, sos))
}

fn parse_component(input: &[u8]) -> IResult<&[u8], Component> {
    let (input, identifier) = be_u8(input)?;
    let (input, sampling_factor) = be_u8(input)?;
    let (input, quantization_table) = be_u8(input)?;
    let component = Component {
        identifier,
        sampling_factor,
        quantization_table,
    };
    Ok((input, component))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return;
    }
    let file_path = Path::new(&args[1]);
    let file = File::open(file_path).unwrap();
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input).unwrap();
    let result = parse_jpeg_file(&input);
    match result {
        Ok((remaining, jpeg_file)) => {
            dbg!(jpeg_file);
            if !remaining.is_empty() {
                eprintln!("Warning: {} bytes remaining in input", remaining.len());
            }
        }
        Err(err) => {
            eprintln!("Error parsing JPEG file: {:?}", err);
        }
    }
}