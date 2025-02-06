use nom::{
    bytes::complete::take,
    combinator::map,
    multi::length_count,
    number::complete::{le_u16, le_u32, le_u64, le_u8},
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
    sequence: u32,
}

#[derive(Debug)]
struct TransactionOutput {
    amount: u64,
    script_pubkey_length: u64,
    script_pubkey: Vec<u8>,
}

#[derive(Debug)]
struct Transaction {
    version: u32,
    inputs: Vec<TransactionInput>,
    outputs: Vec<TransactionOutput>,
    locktime: u32,
}

fn parse_varint(input: &[u8]) -> IResult<&[u8], u64> {
    let (input, first_byte) = le_u8(input)?;
    match first_byte {
        0..=0xFC => Ok((input, first_byte as u64)),
        0xFD => map(le_u16, |x| x as u64)(input),
        0xFE => map(le_u32, |x| x as u64)(input),
        0xFF => le_u64(input),
    }
}

fn parse_transaction_input(input: &[u8]) -> IResult<&[u8], TransactionInput> {
    let (input, prev_tx_hash) = take(32usize)(input)?;
    let (input, prev_output_index) = le_u32(input)?;
    let (input, script_sig_length) = parse_varint(input)?;
    let (input, script_sig) = take(script_sig_length as usize)(input)?;
    let (input, sequence) = le_u32(input)?;

    Ok((input, TransactionInput {
        prev_tx_hash: prev_tx_hash.try_into().unwrap(),
        prev_output_index,
        script_sig_length,
        script_sig: script_sig.to_vec(),
        sequence,
    }))
}

fn parse_transaction_output(input: &[u8]) -> IResult<&[u8], TransactionOutput> {
    let (input, amount) = le_u64(input)?;
    let (input, script_pubkey_length) = parse_varint(input)?;
    let (input, script_pubkey) = take(script_pubkey_length as usize)(input)?;

    Ok((input, TransactionOutput {
        amount,
        script_pubkey_length,
        script_pubkey: script_pubkey.to_vec(),
    }))
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    let (input, version) = le_u32(input)?;
    let (input, inputs) = length_count(parse_varint, parse_transaction_input)(input)?;
    let (input, outputs) = length_count(parse_varint, parse_transaction_output)(input)?;
    let (input, locktime) = le_u32(input)?;

    Ok((input, Transaction {
        version,
        inputs,
        outputs,
        locktime,
    }))
}

fn main() -> std::io::Result<()> {
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
            std::process::exit(1)
        }
    }
}