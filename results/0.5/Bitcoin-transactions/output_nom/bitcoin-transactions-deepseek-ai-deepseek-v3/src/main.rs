use nom::{
    bytes::complete::take,
    number::complete::{le_u32, le_u64},
    multi::length_data,
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct Transaction {
    version: u32,
    inputs: Vec<Input>,
    outputs: Vec<Output>,
    lock_time: u32,
    witness_data: Option<Vec<Witness>>,
}

#[derive(Debug)]
struct Input {
    previous_tx_hash: Vec<u8>,
    previous_output_index: u32,
    script: Vec<u8>,
    sequence: u32,
}

#[derive(Debug)]
struct Output {
    value: u64,
    script: Vec<u8>,
}

#[derive(Debug)]
struct Witness {
    elements: Vec<Vec<u8>>,
}

fn parse_varint(input: &[u8]) -> IResult<&[u8], u64> {
    let (input, first_byte) = take(1u8)(input)?;
    let first_byte = first_byte[0];
    match first_byte {
        0xFD => take(2u8)(input).map(|(i, bytes)| (i, u16::from_le_bytes(bytes.try_into().unwrap()) as u64)),
        0xFE => take(4u8)(input).map(|(i, bytes)| (i, u32::from_le_bytes(bytes.try_into().unwrap()) as u64)),
        0xFF => take(8u8)(input).map(|(i, bytes)| (i, u64::from_le_bytes(bytes.try_into().unwrap()))),
        _ => Ok((input, first_byte as u64)),
    }
}

fn parse_input(input: &[u8]) -> IResult<&[u8], Input> {
    let (input, previous_tx_hash) = take(32u8)(input)?;
    let (input, previous_output_index) = le_u32(input)?;
    let (input, script) = length_data(parse_varint)(input)?;
    let (input, sequence) = le_u32(input)?;
    Ok((input, Input {
        previous_tx_hash: previous_tx_hash.to_vec(),
        previous_output_index,
        script: script.to_vec(),
        sequence,
    }))
}

fn parse_output(input: &[u8]) -> IResult<&[u8], Output> {
    let (input, value) = le_u64(input)?;
    let (input, script) = length_data(parse_varint)(input)?;
    Ok((input, Output {
        value,
        script: script.to_vec(),
    }))
}

fn parse_witness(input: &[u8]) -> IResult<&[u8], Witness> {
    let (input, count) = parse_varint(input)?;
    let (input, elements) = nom::multi::count(length_data(parse_varint), count as usize)(input)?;
    Ok((input, Witness {
        elements: elements.into_iter().map(|e| e.to_vec()).collect(),
    }))
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    let (input, version) = le_u32(input)?;
    let (input, marker) = take(1u8)(input)?;
    let (input, flag) = take(1u8)(input)?;
    let is_segwit = marker[0] == 0x00 && flag[0] == 0x01;
    let (input, inputs) = if is_segwit {
        let (input, input_count) = parse_varint(input)?;
        let (input, inputs) = nom::multi::count(parse_input, input_count as usize)(input)?;
        (input, inputs)
    } else {
        let (input, input_count) = parse_varint(input)?;
        let (input, inputs) = nom::multi::count(parse_input, input_count as usize)(input)?;
        (input, inputs)
    };
    let (input, outputs) = {
        let (input, output_count) = parse_varint(input)?;
        let (input, outputs) = nom::multi::count(parse_output, output_count as usize)(input)?;
        (input, outputs)
    };
    let (input, witness_data) = if is_segwit {
        let (input, witness_count) = parse_varint(input)?;
        let (input, witness_data) = nom::multi::count(parse_witness, witness_count as usize)(input)?;
        (input, Some(witness_data))
    } else {
        (input, None)
    };
    let (input, lock_time) = le_u32(input)?;
    Ok((input, Transaction {
        version,
        inputs,
        outputs,
        lock_time,
        witness_data,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        return;
    }
    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");
    match parse_transaction(&buffer) {
        Ok((_, transaction)) => println!("{:?}", transaction),
        Err(e) => eprintln!("Failed to parse transaction: {:?}", e),
    }
}