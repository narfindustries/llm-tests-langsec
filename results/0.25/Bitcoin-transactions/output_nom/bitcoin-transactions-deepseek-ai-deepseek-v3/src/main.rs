use nom::{
    bytes::complete::take,
    number::complete::{le_u32, le_u64},
    multi::length_data,
    sequence::tuple,
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug)]
struct Transaction {
    version: u32,
    inputs: Vec<Input>,
    outputs: Vec<Output>,
    lock_time: u32,
    witness: Option<Vec<Vec<u8>>>,
}

#[derive(Debug)]
struct Input {
    previous_tx_hash: Vec<u8>,
    previous_output_index: u32,
    script_sig: Vec<u8>,
    sequence: u32,
}

#[derive(Debug)]
struct Output {
    value: u64,
    script_pubkey: Vec<u8>,
}

fn parse_varint(input: &[u8]) -> IResult<&[u8], usize> {
    let (input, first_byte) = take(1usize)(input)?;
    let first_byte = first_byte[0];
    match first_byte {
        0xFD => {
            let (input, val) = take(2usize)(input)?;
            Ok((input, u16::from_le_bytes(val.try_into().unwrap()) as usize))
        }
        0xFE => {
            let (input, val) = take(4usize)(input)?;
            Ok((input, u32::from_le_bytes(val.try_into().unwrap()) as usize))
        }
        0xFF => {
            let (input, val) = take(8usize)(input)?;
            Ok((input, u64::from_le_bytes(val.try_into().unwrap()) as usize))
        }
        _ => Ok((input, first_byte as usize)),
    }
}

fn parse_input(input: &[u8]) -> IResult<&[u8], Input> {
    let (input, previous_tx_hash) = take(32usize)(input)?;
    let (input, previous_output_index) = le_u32(input)?;
    let (input, script_sig) = length_data(parse_varint)(input)?;
    let (input, sequence) = le_u32(input)?;
    Ok((
        input,
        Input {
            previous_tx_hash: previous_tx_hash.to_vec(),
            previous_output_index,
            script_sig: script_sig.to_vec(),
            sequence,
        },
    ))
}

fn parse_output(input: &[u8]) -> IResult<&[u8], Output> {
    let (input, value) = le_u64(input)?;
    let (input, script_pubkey) = length_data(parse_varint)(input)?;
    Ok((
        input,
        Output {
            value,
            script_pubkey: script_pubkey.to_vec(),
        },
    ))
}

fn parse_witness(input: &[u8], witness_count: usize) -> IResult<&[u8], Vec<Vec<u8>>> {
    let mut witnesses = Vec::new();
    let mut remaining_input = input;
    for _ in 0..witness_count {
        let (input, witness) = length_data(parse_varint)(remaining_input)?;
        witnesses.push(witness.to_vec());
        remaining_input = input;
    }
    Ok((remaining_input, witnesses))
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    let (input, version) = le_u32(input)?;
    let (input, input_count) = parse_varint(input)?;
    let (input, inputs) = nom::multi::many_m_n(input_count, input_count, parse_input)(input)?;
    let (input, output_count) = parse_varint(input)?;
    let (input, outputs) = nom::multi::many_m_n(output_count, output_count, parse_output)(input)?;
    let (input, witness) = if input.starts_with(&[0x00, 0x01]) {
        let (input, _) = take(2usize)(input)?;
        let (input, witness_count) = parse_varint(input)?;
        let (input, witness) = parse_witness(input, witness_count)?;
        (input, Some(witness))
    } else {
        (input, None)
    };
    let (input, lock_time) = le_u32(input)?;
    Ok((
        input,
        Transaction {
            version,
            inputs,
            outputs,
            lock_time,
            witness,
        },
    ))
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
        Ok((_, transaction)) => println!("{:#?}", transaction),
        Err(e) => eprintln!("Failed to parse transaction: {:?}", e),
    }
}