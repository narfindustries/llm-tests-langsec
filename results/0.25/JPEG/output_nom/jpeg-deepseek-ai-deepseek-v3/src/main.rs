use nom::{
    bytes::complete::{tag, take},
    combinator::map,
    multi::many0,
    number::complete::{be_u8, be_u16},
    sequence::{pair, preceded, tuple},
    IResult,
};
use std::env;
use std::fs;

#[derive(Debug)]
struct JPEG {
    soi: SOI,
    segments: Vec<Segment>,
    eoi: EOI,
}

#[derive(Debug)]
struct SOI;

#[derive(Debug)]
struct EOI;

#[derive(Debug)]
enum Segment {
    SOF(SOF),
    DHT(DHT),
    DQT(DQT),
    SOS(SOS),
    APPn(APPn),
    COM(COM),
    DRI(DRI),
    DAC(DAC),
    DHP(DHP),
    EXP(EXP),
}

#[derive(Debug)]
struct SOF {
    precision: u8,
    height: u16,
    width: u16,
    components: Vec<Component>,
}

#[derive(Debug)]
struct Component {
    id: u8,
    sampling_factors: u8,
    quantization_table_id: u8,
}

#[derive(Debug)]
struct DHT {
    tables: Vec<HuffmanTable>,
}

#[derive(Debug)]
struct HuffmanTable {
    class: u8,
    destination_id: u8,
    codes: Vec<u8>,
}

#[derive(Debug)]
struct DQT {
    tables: Vec<QuantizationTable>,
}

#[derive(Debug)]
struct QuantizationTable {
    precision: u8,
    destination_id: u8,
    table: Vec<u8>,
}

#[derive(Debug]
struct SOS {
    components: Vec<SOSComponent>,
    spectral_selection_start: u8,
    spectral_selection_end: u8,
    successive_approximation: u8,
}

#[derive(Debug)]
struct SOSComponent {
    component_id: u8,
    entropy_coding_table_selector: u8,
}

#[derive(Debug)]
struct APPn {
    identifier: u8,
    data: Vec<u8>,
}

#[derive(Debug)]
struct COM {
    comment: Vec<u8>,
}

#[derive(Debug)]
struct DRI {
    restart_interval: u16,
}

#[derive(Debug)]
struct DAC {
    parameters: Vec<u8>,
}

#[derive(Debug)]
struct DHP {
    parameters: u16,
}

#[derive(Debug)]
struct EXP {
    parameters: u8,
}

fn parse_soi(input: &[u8]) -> IResult<&[u8], SOI> {
    map(tag([0xFF, 0xD8]), |_| SOI)(input)
}

fn parse_eoi(input: &[u8]) -> IResult<&[u8], EOI> {
    map(tag([0xFF, 0xD9]), |_| EOI)(input)
}

fn parse_sof(input: &[u8]) -> IResult<&[u8], SOF> {
    let (input, (precision, height, width, component_count)) =
        tuple((be_u8, be_u16, be_u16, be_u8))(input)?;
    let (input, components) = take(component_count as usize * 3)(input)?;
    let components = components
        .chunks(3)
        .map(|chunk| Component {
            id: chunk[0],
            sampling_factors: chunk[1],
            quantization_table_id: chunk[2],
        })
        .collect();
    Ok((input, SOF {
        precision,
        height,
        width,
        components,
    }))
}

fn parse_dht(input: &[u8]) -> IResult<&[u8], DHT> {
    let (input, tables) = many0(parse_huffman_table)(input)?;
    Ok((input, DHT { tables }))
}

fn parse_huffman_table(input: &[u8]) -> IResult<&[u8], HuffmanTable> {
    let (input, (class, destination_id)) = tuple((be_u8, be_u8))(input)?;
    let (input, codes) = take(16)(input)?;
    Ok((input, HuffmanTable {
        class,
        destination_id,
        codes: codes.to_vec(),
    }))
}

fn parse_dqt(input: &[u8]) -> IResult<&[u8], DQT> {
    let (input, tables) = many0(parse_quantization_table)(input)?;
    Ok((input, DQT { tables }))
}

fn parse_quantization_table(input: &[u8])