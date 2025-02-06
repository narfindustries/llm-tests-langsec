use nom::{
    bytes::complete::take,
    combinator::map,
    multi::many1,
    number::complete::{le_u16, le_u32, le_u64},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct TransactionInput {
    prev_tx_hash: [u8; 32],
    prev_output_index: u32,
    script_sig_length: u64,
    script_sig: Vec<u8>,
    sequence_number: u32,
}

#[derive(Debug)]
struct TransactionOutput {
    value: u64,
    script_pubkey_length: u64,
    script_pubkey: Vec<u8>,
}

#[derive(Debug)]
struct Transaction {
    version: u32,
    input_count: u64,
    inputs: Vec<TransactionInput>,
    output_count: u64,
    outputs: Vec<TransactionOutput>,
    locktime: u32,
}

fn parse_varint(input: &[u8]) -> IResult<&[u8], u64> {
    let (input, first_byte) = take(1usize)(input)?;
    match first_byte[0] {
        x if x < 0xFD => Ok((input, x as u64)),
        0xFD => map(le_u16, |v| v as u64)(input),
        0xFE => map(le_u32, |v| v as u64)(input),
        0xFF => le_u64(input),
        _ => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Fail))),
    }
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
    let (input, inputs) = many1(parse_transaction_input)(input)?;
    let (input, output_count) = parse_varint(input)?;
    let (input, outputs) = many1(parse_transaction_output)(input)?;
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
            Err("Parsing error".into())
        }
    }
}