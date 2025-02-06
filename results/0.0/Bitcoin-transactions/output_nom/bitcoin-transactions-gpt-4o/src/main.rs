use std::fs::File;
use std::io::Read;
use std::env;
use nom::{
    IResult,
    bytes::complete::take,
    number::complete::{le_u32, le_u64, le_u8, le_u16},
    multi::length_data,
};

#[derive(Debug)]
struct TransactionInput {
    previous_transaction_hash: [u8; 32],
    output_index: u32,
    script_sig: Vec<u8>,
    sequence: u32,
}

#[derive(Debug)]
struct TransactionOutput {
    value: u64,
    script_pubkey: Vec<u8>,
}

#[derive(Debug)]
struct BitcoinTransaction {
    version: u32,
    inputs: Vec<TransactionInput>,
    outputs: Vec<TransactionOutput>,
    locktime: u32,
}

fn parse_varint(input: &[u8]) -> IResult<&[u8], u64> {
    let (input, first_byte) = le_u8(input)?;
    match first_byte {
        0xFD => {
            let (input, value) = le_u16(input)?;
            Ok((input, value as u64))
        },
        0xFE => {
            let (input, value) = le_u32(input)?;
            Ok((input, value as u64))
        },
        0xFF => {
            let (input, value) = le_u64(input)?;
            Ok((input, value))
        },
        _ => Ok((input, first_byte as u64)),
    }
}

fn parse_transaction_input(input: &[u8]) -> IResult<&[u8], TransactionInput> {
    let (input, previous_transaction_hash) = take(32usize)(input)?;
    let (input, output_index) = le_u32(input)?;
    let (input, script_sig) = length_data(parse_varint)(input)?;
    let (input, sequence) = le_u32(input)?;

    Ok((input, TransactionInput {
        previous_transaction_hash: previous_transaction_hash.try_into().unwrap(),
        output_index,
        script_sig: script_sig.to_vec(),
        sequence,
    }))
}

fn parse_transaction_output(input: &[u8]) -> IResult<&[u8], TransactionOutput> {
    let (input, value) = le_u64(input)?;
    let (input, script_pubkey) = length_data(parse_varint)(input)?;

    Ok((input, TransactionOutput {
        value,
        script_pubkey: script_pubkey.to_vec(),
    }))
}

fn parse_bitcoin_transaction(input: &[u8]) -> IResult<&[u8], BitcoinTransaction> {
    let (input, version) = le_u32(input)?;
    let (input, input_count) = parse_varint(input)?;
    let (input, inputs) = nom::multi::count(parse_transaction_input, input_count as usize)(input)?;
    let (input, output_count) = parse_varint(input)?;
    let (input, outputs) = nom::multi::count(parse_transaction_output, output_count as usize)(input)?;
    let (input, locktime) = le_u32(input)?;

    Ok((input, BitcoinTransaction {
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

    match parse_bitcoin_transaction(&buffer) {
        Ok((_, transaction)) => println!("{:?}", transaction),
        Err(e) => eprintln!("Failed to parse transaction: {:?}", e),
    }
}