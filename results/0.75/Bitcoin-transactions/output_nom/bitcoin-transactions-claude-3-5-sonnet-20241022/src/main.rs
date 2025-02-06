use nom::{
    bytes::complete::{tag, take},
    multi::{length_data, many0},
    number::complete::{le_u32, le_u64, le_u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct Transaction {
    version: u32,
    inputs: Vec<TxInput>,
    outputs: Vec<TxOutput>,
    locktime: u32,
}

#[derive(Debug)]
struct TxInput {
    prev_tx_hash: [u8; 32],
    prev_output_index: u32,
    script: Vec<u8>,
    sequence: u32,
}

#[derive(Debug)]
struct TxOutput {
    amount: u64,
    script: Vec<u8>,
}

fn parse_varint(input: &[u8]) -> IResult<&[u8], u64> {
    let (input, first_byte) = le_u8(input)?;
    match first_byte {
        0xff => {
            let (input, value) = le_u64(input)?;
            Ok((input, value))
        }
        0xfe => {
            let (input, value) = le_u32(input)?;
            Ok((input, value as u64))
        }
        0xfd => {
            let (input, value) = le_u16(input)?;
            Ok((input, value as u64))
        }
        _ => Ok((input, first_byte as u64)),
    }
}

fn parse_tx_input(input: &[u8]) -> IResult<&[u8], TxInput> {
    let (input, prev_tx_hash) = take(32usize)(input)?;
    let (input, prev_output_index) = le_u32(input)?;
    let (input, script_length) = parse_varint(input)?;
    let (input, script) = take(script_length as usize)(input)?;
    let (input, sequence) = le_u32(input)?;

    Ok((
        input,
        TxInput {
            prev_tx_hash: prev_tx_hash.try_into().unwrap(),
            prev_output_index,
            script: script.to_vec(),
            sequence,
        },
    ))
}

fn parse_tx_output(input: &[u8]) -> IResult<&[u8], TxOutput> {
    let (input, amount) = le_u64(input)?;
    let (input, script_length) = parse_varint(input)?;
    let (input, script) = take(script_length as usize)(input)?;

    Ok((
        input,
        TxOutput {
            amount,
            script: script.to_vec(),
        },
    ))
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    let (input, version) = le_u32(input)?;
    let (input, input_count) = parse_varint(input)?;
    let (input, inputs) = many0(parse_tx_input)(input)?;
    let (input, output_count) = parse_varint(input)?;
    let (input, outputs) = many0(parse_tx_output)(input)?;
    let (input, locktime) = le_u32(input)?;

    Ok((
        input,
        Transaction {
            version,
            inputs,
            outputs,
            locktime,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <bitcoin_tx_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_transaction(&buffer) {
        Ok((remaining, transaction)) => {
            println!("Successfully parsed transaction: {:?}", transaction);
            println!("Remaining bytes: {} bytes", remaining.len());
        }
        Err(e) => {
            eprintln!("Failed to parse transaction: {:?}", e);
            std::process::exit(1);
        }
    }
}

fn le_u16(input: &[u8]) -> IResult<&[u8], u16> {
    let (input, bytes) = take(2usize)(input)?;
    Ok((input, u16::from_le_bytes(bytes.try_into().unwrap())))
}