use nom::{
    bytes::complete::{tag, take},
    combinator::map,
    multi::take_while_m_n,
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::{
    env, fs,
    io::{self, Read},
    str,
};

#[derive(Debug)]
struct JpegFile {
    sof0: SOF0,
    dht: Option<Vec<DHT>>,
    dqt: Option<Vec<DQT>>,
    sof2: Option<SOF2>,
    sos: SOS,
}

#[derive(Debug)]
struct SOF0 {
    length: u16,
    precision: u8,
    height: u16,
    width: u16,
    num_components: u8,
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
    table_class: u8,
    table_id: u8,
    table: Vec<u8>,
}

#[derive(Debug)]
struct DQT {
    precision: u8,
    table_id: u8,
    table: Vec<u8>,
}

#[derive(Debug)]
struct SOF2 {
    length: u16,
    num_components: u8,
    components: Vec<Component>,
}

#[derive(Debug)]
struct SOS {
    length: u16,
    num_components: u8,
    components: Vec<u8>,
    spectral_selection_start: u8,
    spectral_selection_end: u8,
    successive_approximation_bit_position: u8,
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], JpegFile> {
    let (input, _) = tag(b"\xFF\xD8\xFF\xE0")(input)?;

    let (input, length) = be_u16(input)?;
    let (input, _) = take(length as usize - 2)(input)?;

    let (input, sof0) = parse_sof0(input)?;

    let (input, dht) = parse_dht(input)?;
    let (input, dqt) = parse_dqt(input)?;
    let (input, sof2) = parse_sof2(input)?;

    let (input, sos) = parse_sos(input)?;

    Ok((input, JpegFile { sof0, dht, dqt, sof2, sos }))
}

fn parse_sof0(input: &[u8]) -> IResult<&[u8], SOF0> {
    let (input, _) = tag(b"\xFF\xC0")(input)?;
    let (input, length) = be_u16(input)?;
    let (input, precision) = be_u8(input)?;
    let (input, height) = be_u16(input)?;
    let (input, width) = be_u16(input)?;
    let (input, num_components) = be_u8(input)?;
    let (input, components) = take_while_m_n(num_components as usize, num_components as usize, is_component)(input)?;

    let components: Vec<Component> = components
        .chunks(3)
        .map(|chunk| Component {
            id: chunk[0],
            sampling_factors: chunk[1],
            quantization_table_id: chunk[2],
        })
        .collect();

    Ok((input, SOF0 { length, precision, height, width, num_components, components }))
}

fn is_component(input: u8) -> bool {
    input >= 1 && input <= 4
}

fn parse_dht(input: &[u8]) -> IResult<&[u8], Option<Vec<DHT>>> {
    let (mut input, dht) = map(
        take_while_m_n(0, 100, |input| *input == 0xFF && input[1] == 0xC4),
        |dht: &[u8]| {
            let mut dhts = Vec::new();
            let mut dht = dht;
            while !dht.is_empty() {
                let (rest, dht_chunk) = take(65)(dht)?;
                let (table_class, table_id, table) = (&dht_chunk[1..2], &dht_chunk[2..3], &dht_chunk[3..]);
                dhts.push(DHT {
                    table_class: table_class[0],
                    table_id: table_id[0],
                    table: table.to_vec(),
                });
                dht = rest;
            }
            Some(dhts)
        },
    )(input)?;

    Ok((input, dht))
}

fn parse_dqt(input: &[u8]) -> IResult<&[u8], Option<Vec<DQT>>> {
    let (mut input, dqt) = map(
        take_while_m_n(0, 100, |input| *input == 0xFF && input[1] == 0xDB),
        |dqt: &[u8]| {
            let mut dqts = Vec::new();
            let mut dqt = dqt;
            while !dqt.is_empty() {
                let (rest, dqt_chunk) = take(65)(dqt)?;
                let (precision, table_id, table) = (&dqt_chunk[1..2], &dqt_chunk[2..3], &dqt_chunk[3..]);
                dqts.push(DQT {
                    precision: precision[0],
                    table_id: table_id[0],
                    table: table.to_vec(),
                });
                dqt = rest;
            }
            Some(dqts)
        },
    )(input)?;

    Ok((input, dqt))
}

fn parse_sof2(input: &[u8]) -> IResult<&[u8], Option<SOF2>> {
    let (input, sof2) = map(
        take_while_m_n(0, 100, |input| *input == 0xFF && input[1] == 0xC2),
        |sof2: &[u8]| {
            if sof2.is_empty() {
                None
            } else {
                let length = u16::from_be_bytes([sof2[2], sof2[3]]);
                let num_components = sof2[4];
                let components: Vec<Component> = sof2[5..5 + (num_components as usize * 3)]
                    .chunks(3)
                    .map(|chunk| Component {
                        id: chunk[0],
                        sampling_factors: chunk[1],
                        quantization_table_id: chunk[2],
                    })
                    .collect();

                Some(SOF2 {
                    length,
                    num_components,
                    components,
                })
            }
        },
    )(input)?;

    Ok((input, sof2))
}

fn parse_sos(input: &[u8]) -> IResult<&[u8], SOS> {
    let (input, _) = tag(b"\xFF\xDA")(input)?;
    let (input, length) = be_u16(input)?;
    let (input, num_components) = be_u8(input)?;
    let (input, components) = take(num_components as usize)(input)?;
    let (input, spectral_selection_start) = be_u8(input)?;
    let (input, spectral_selection_end) = be_u8(input)?;
    let (input, successive_approximation_bit_position) = be_u8(input)?;

    Ok((input, SOS {
        length,
        num_components,
        components: components.to_vec(),
        spectral_selection_start,
        spectral_selection_end,
        successive_approximation_bit_position,
    }))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return Ok(());
    }

    let mut file = fs::File::open(&args[1])?;
    let mut input = Vec::new();
    file.read_to_end(&mut input)?;

    let (_, jpeg) = parse_jpeg(&input).unwrap();

    println!("{:?}", jpeg);

    Ok(())
}