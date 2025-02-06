// Add dependencies in Cargo.toml
// [dependencies]
// nom = "7.0"
// hex = "0.4.3"
// std = { version = "0.3", features = ["alloc"] }

use nom::{
    bytes::complete::take,
    combinator::{map, map_parser},
    multi::{count, length_data},
    number::complete::{le_u16, le_u32, le_u64, le_u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

// Types to hold data structures
#[derive(Debug)]
struct TransactionInput {
    previous_hash: [u8; 32],
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
struct Transaction {
    version: u32,
    inputs: Vec<TransactionInput>,
    outputs: Vec<TransactionOutput>,
    locktime: u32,
}

// Helper parsers
fn varint(input: &[u8]) -> IResult<&[u8], u64> {
    let (input, first) = le_u8(input)?;
    match first {
        0xFD => map(le_u16, u64::from)(input),
        0xFE => map(le_u32, u64::from)(input),
        0xFF => map(le_u64, u64::from)(input),
        n => Ok((input, n as u64)),
    }
}

fn parse_u256(input: &[u8]) -> IResult<&[u8], [u8; 32]> {
    map(take(32usize), |slice: &[u8]| {
        let mut array = [0u8; 32];
        array.copy_from_slice(slice);
        array
    })(input)
}

fn parse_transaction_input(input: &[u8]) -> IResult<&[u8], TransactionInput> {
    let (input, previous_hash) = parse_u256(input)?;
    let (input, output_index) = le_u32(input)?;
    let (input, script_sig) = length_data(varint)(input)?;
    let (input, sequence) = le_u32(input)?;
    Ok((
        input,
        TransactionInput {
            previous_hash,
            output_index,
            script_sig: script_sig.to_vec(),
            sequence,
        },
    ))
}

fn parse_transaction_output(input: &[u8]) -> IResult<&[u8], TransactionOutput> {
    let (input, value) = le_u64(input)?;
    let (input, script_pubkey) = length_data(varint)(input)?;
    Ok((
        input,
        TransactionOutput {
            value,
            script_pubkey: script_pubkey.to_vec(),
        },
    ))
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    let (input, version) = le_u32(input)?;
    let (input, input_count) = varint(input)?;
    let (input, inputs) = count(parse_transaction_input, input_count as usize)(input)?;
    let (input, output_count) = varint(input)?;
    let (input, outputs) = count(parse_transaction_output, output_count as usize)(input)?;
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
        eprintln!("Usage: {} <transaction_file>", args[0]);
        std::process::exit(1);
    }

    let file_name = &args[1];
    let mut file = File::open(file_name).expect("Unable to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Unable to read data");

    match parse_transaction(&buffer) {
        Ok((_, transaction)) => println!("{:#?}", transaction),
        Err(err) => eprintln!("Error parsing transaction: {:?}", err),
    }
}