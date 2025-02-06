use nom::{
    bytes::complete::take,
    number::complete::{le_i32, le_u32, le_u64, le_u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct Transaction {
    version: i32,
    inputs: Vec<TxInput>,
    outputs: Vec<TxOutput>,
    lock_time: u32,
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
    value: u64,
    script: Vec<u8>,
}

fn parse_varint(input: &[u8]) -> IResult<&[u8], u64> {
    let (input, first_byte) = le_u8(input)?;
    match first_byte {
        0xff => {
            let (input, n) = le_u64(input)?;
            Ok((input, n))
        }
        0xfe => {
            let (input, n) = le_u32(input)?;
            Ok((input, n as u64))
        }
        0xfd => {
            let (input, n) = le_u16(input)?;
            Ok((input, n as u64))
        }
        n => Ok((input, n as u64)),
    }
}

fn parse_tx_input(input: &[u8]) -> IResult<&[u8], TxInput> {
    let (input, prev_tx_hash_slice) = take(32usize)(input)?;
    let mut prev_tx_hash = [0u8; 32];
    prev_tx_hash.copy_from_slice(prev_tx_hash_slice);
    
    let (input, prev_output_index) = le_u32(input)?;
    let (input, script_len) = parse_varint(input)?;
    let (input, script) = take(script_len as usize)(input)?;
    let (input, sequence) = le_u32(input)?;

    Ok((input, TxInput {
        prev_tx_hash,
        prev_output_index,
        script: script.to_vec(),
        sequence,
    }))
}

fn parse_tx_output(input: &[u8]) -> IResult<&[u8], TxOutput> {
    let (input, value) = le_u64(input)?;
    let (input, script_len) = parse_varint(input)?;
    let (input, script) = take(script_len as usize)(input)?;

    Ok((input, TxOutput {
        value,
        script: script.to_vec(),
    }))
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    let (input, version) = le_i32(input)?;
    let (input, input_count) = parse_varint(input)?;
    
    let mut inputs = Vec::new();
    let mut remaining = input;
    for _ in 0..input_count {
        let (input, tx_input) = parse_tx_input(remaining)?;
        inputs.push(tx_input);
        remaining = input;
    }

    let (input, output_count) = parse_varint(remaining)?;
    
    let mut outputs = Vec::new();
    let mut remaining = input;
    for _ in 0..output_count {
        let (input, tx_output) = parse_tx_output(remaining)?;
        outputs.push(tx_output);
        remaining = input;
    }

    let (input, lock_time) = le_u32(remaining)?;

    Ok((input, Transaction {
        version,
        inputs,
        outputs,
        lock_time,
    }))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <transaction_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_transaction(&buffer) {
        Ok((remaining, transaction)) => {
            println!("Parsed transaction: {:?}", transaction);
            if !remaining.is_empty() {
                println!("Warning: {} bytes remaining", remaining.len());
            }
        }
        Err(e) => {
            eprintln!("Error parsing transaction: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}

fn le_u16(input: &[u8]) -> IResult<&[u8], u16> {
    let (input, bytes) = take(2usize)(input)?;
    Ok((input, u16::from_le_bytes([bytes[0], bytes[1]])))
}