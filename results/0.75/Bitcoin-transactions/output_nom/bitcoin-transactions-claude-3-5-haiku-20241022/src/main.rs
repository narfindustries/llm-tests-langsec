use nom::{
    branch::alt,
    bytes::complete::{tag, take},
    combinator::{map, map_opt},
    multi::{count, many0, many1},
    number::complete::{le_u8, le_u16, le_u32, le_u64},
    sequence::{preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug, Clone)]
struct TransactionInput {
    prev_tx_hash: [u8; 32],
    prev_output_index: u32,
    script_sig_length: u64,
    script_sig: Vec<u8>,
    sequence_number: u32,
}

#[derive(Debug, Clone)]
struct TransactionOutput {
    value: u64,
    script_pubkey_length: u64,
    script_pubkey: Vec<u8>,
}

#[derive(Debug, Clone)]
struct Transaction {
    version: u32,
    input_count: u64,
    inputs: Vec<TransactionInput>,
    output_count: u64,
    outputs: Vec<TransactionOutput>,
    locktime: u32,
}

fn parse_varint(input: &[u8]) -> IResult<&[u8], u64> {
    alt((
        map(le_u8, |v| v as u64),
        map(preceded(tag(&[0xfd]), le_u16), |v| v as u64),
        map(preceded(tag(&[0xfe]), le_u32), |v| v as u64),
        map(preceded(tag(&[0xff]), le_u64), |v| v),
    ))(input)
}

fn parse_transaction_input(input: &[u8]) -> IResult<&[u8], TransactionInput> {
    let (input, prev_tx_hash) = take(32usize)(input)?;
    let (input, prev_output_index) = le_u32(input)?;
    let (input, script_sig_length) = parse_varint(input)?;
    let (input, script_sig) = take(script_sig_length as usize)(input)?;
    let (input, sequence_number) = le_u32(input)?;

    Ok((input, TransactionInput {
        prev_tx_hash: prev_tx_hash.try_into().unwrap(),
        prev_output_index,
        script_sig_length,
        script_sig: script_sig.to_vec(),
        sequence_number,
    }))
}

fn parse_transaction_output(input: &[u8]) -> IResult<&[u8], TransactionOutput> {
    let (input, value) = le_u64(input)?;
    let (input, script_pubkey_length) = parse_varint(input)?;
    let (input, script_pubkey) = take(script_pubkey_length as usize)(input)?;

    Ok((input, TransactionOutput {
        value,
        script_pubkey_length,
        script_pubkey: script_pubkey.to_vec(),
    }))
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    let (input, version) = le_u32(input)?;
    let (input, input_count) = parse_varint(input)?;
    let (input, inputs) = count(parse_transaction_input, input_count as usize)(input)?;
    let (input, output_count) = parse_varint(input)?;
    let (input, outputs) = count(parse_transaction_output, output_count as usize)(input)?;
    let (input, locktime) = le_u32(input)?;

    Ok((input, Transaction {
        version,
        input_count,
        inputs,
        output_count,
        outputs,
        locktime,
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
        Ok((_, transaction)) => {
            println!("Parsed Transaction: {:?}", transaction);
            Ok(())
        }
        Err(e) => {
            eprintln!("Failed to parse transaction: {:?}", e);
            std::process::exit(1);
        }
    }
}