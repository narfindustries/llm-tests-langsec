use nom::{
    bytes::complete::{tag, take},
    combinator::map,
    error::ParseError,
    multi::many0,
    number::complete::be_u16,
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::path::Path;

#[derive(Debug)]
struct JpegHeader {
    soi: (),
    app: Vec<Vec<u8>>,
    dqt: Vec<QuantizationTable>,
    dht: Vec<HuffmanTable>,
    sof: StartOfFrame,
    sos: StartOfScan,
    eoi: (),
}

#[derive(Debug)]
struct QuantizationTable {
    precision: u8,
    table_data: Vec<u16>,
}

#[derive(Debug)]
struct HuffmanTable {
    table_class: u8,
    table_id: u8,
    code_lengths: Vec<u8>,
    huffman_codes: Vec<u16>,
}

#[derive(Debug)]
struct StartOfFrame {
    baseline: u8,
    image_height: u16,
    image_width: u16,
    number_of_components: u8,
    components: Vec<Component>,
}

#[derive(Debug)]
struct Component {
    component_id: u8,
    horizontal_sampling_factor: u8,
    vertical_sampling_factor: u8,
    quantization_table_id: u8,
}

#[derive(Debug)]
struct StartOfScan {
    number_of_components: u8,
    components: Vec<ScanComponent>,
    spectral_selection_start: u8,
    spectral_selection_end: u8,
    successive_approximation: u8,
}

#[derive(Debug)]
struct ScanComponent {
    component_id: u8,
    dc_entropy_table: u8,
    ac_entropy_table: u8,
}

fn parse_jpeg_header(input: &[u8]) -> IResult<&[u8], JpegHeader> {
    let (input, _) = tag(b"\xFF\xD8")(input)?; //SOI

    let (input, app_data) = many0(parse_app)(input)?;

    Ok((
        input,
        JpegHeader {
            soi: (),
            app: app_data,
            dqt: Vec::new(),
            dht: Vec::new(),
            sof: StartOfFrame {
                baseline: 0,
                image_height: 0,
                image_width: 0,
                number_of_components: 0,
                components: Vec::new(),
            },
            sos: StartOfScan {
                number_of_components: 0,
                components: Vec::new(),
                spectral_selection_start: 0,
                spectral_selection_end: 0,
                successive_approximation: 0,
            },
            eoi: (),
        },
    ))
}

fn parse_app(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, _) = tag(b"\xFF")(input)?;
    let (input, app_data_len) = be_u16(input)?;
    let (input, app_data) = take(app_data_len as usize)(input)?;
    Ok((input, app_data.to_vec()))
}


fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <JPEG_file>", args[0]);
        return;
    }
    let path = Path::new(&args[1]);
    let mut file = match File::open(&path) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Error opening file: {}", err);
            return;
        }
    };
    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Ok(_) => (),
        Err(err) => {
            eprintln!("Error reading file: {}", err);
            return;
        }
    };
    match parse_jpeg_header(&buffer) {
        Ok((_, header)) => println!("JPEG Header: {:?}", header),
        Err(err) => eprintln!("Error parsing JPEG: {:?}", err),
    };
}