use std::fs::File;
use std::io::Read;
use std::env;
use nom::{
    IResult,
    bytes::complete::{take, take_while},
    number::complete::{le_u32, le_u64},
    combinator::map_res,
    multi::length_data,
    sequence::tuple,
};

#[derive(Debug)]
struct TransactionInput {
    previous_transaction_hash: [u8; 32],
    previous_transaction_output_index: u32,
    script_length: u64,
    signature_script: Vec<u8>,
    sequence: u32,
}

#[derive(Debug)]
struct TransactionOutput {
    value: u64,
    script_length: u64,
    script_pub_key: Vec<u8>,
}

#[derive(Debug)]
struct Transaction {
    version: u32,
    inputs: Vec<TransactionInput>,
    outputs: Vec<TransactionOutput>,
    locktime: u32,
}

fn parse_varint(input: &[u8]) -> IResult<&[u8], u64> {
    let (input, first_byte) = take(1usize)(input)?;
    let first_byte = first_byte[0];
    match first_byte {
        0xFD => {
            let (input, value) = le_u32(input)?;
            Ok((input, value as u64))
        }
        0xFE => {
            let (input, value) = le_u32(input)?;
            Ok((input, value as u64))
        }
        0xFF => {
            let (input, value) = le_u64(input)?;
            Ok((input, value))
        }
        _ => Ok((input, first_byte as u64)),
    }
}

fn parse_input(input: &[u8]) -> IResult<&[u8], TransactionInput> {
    let (input, previous_transaction_hash) = take(32usize)(input)?;
    let (input, previous_transaction_output_index) = le_u32(input)?;
    let (input, script_length) = parse_varint(input)?;
    let (input, signature_script) = take(script_length)(input)?;
    let (input, sequence) = le_u32(input)?;

    Ok((input, TransactionInput {
        previous_transaction_hash: previous_transaction_hash.try_into().unwrap(),
        previous_transaction_output_index,
        script_length,
        signature_script: signature_script.to_vec(),
        sequence,
    }))
}

fn parse_output(input: &[u8]) -> IResult<&[u8], TransactionOutput> {
    let (input, value) = le_u64(input)?;
    let (input, script_length) = parse_varint(input)?;
    let (input, script_pub_key) = take(script_length)(input)?;

    Ok((input, TransactionOutput {
        value,
        script_length,
        script_pub_key: script_pub_key.to_vec(),
    }))
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    let (input, version) = le_u32(input)?;
    let (input, input_count) = parse_varint(input)?;
    let (input, inputs) = nom::multi::count(parse_input, input_count as usize)(input)?;
    let (input, output_count) = parse_varint(input)?;
    let (input, outputs) = nom::multi::count(parse_output, output_count as usize)(input)?;
    let (input, locktime) = le_u32(input)?;

    Ok((input, Transaction {
        version,
        inputs,
        outputs,
        locktime,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_transaction(&buffer) {
        Ok((_, transaction)) => println!("{:#?}", transaction),
        Err(e) => eprintln!("Failed to parse transaction: {:?}", e),
    }
}