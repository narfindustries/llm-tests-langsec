use nom::{
    bytes::complete::{tag, take},
    multi::{count, many0},
    number::complete::{le_u8, le_u16, le_u32, le_u64},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct TxInput {
    prev_tx_hash: [u8; 32],
    prev_output_index: u32,
    script_sig_length: u64,
    script_sig: Vec<u8>,
    sequence: u32,
}

#[derive(Debug)]
struct TxOutput {
    value: u64,
    script_pubkey_length: u64,
    script_pubkey: Vec<u8>,
}

#[derive(Debug)]
struct Transaction {
    version: u32,
    input_count: u64,
    inputs: Vec<TxInput>,
    output_count: u64,
    outputs: Vec<TxOutput>,
    locktime: u32,
}

fn parse_tx_input(input: &[u8]) -> IResult<&[u8], TxInput> {
    let (input, (prev_tx_hash, prev_output_index)) = tuple((take(32usize), le_u32))(input)?;
    let (input, script_sig_length) = le_u64(input)?;
    let (input, script_sig) = take(script_sig_length)(input)?;
    let (input, sequence) = le_u32(input)?;

    Ok((input, TxInput {
        prev_tx_hash: prev_tx_hash.try_into().unwrap(),
        prev_output_index,
        script_sig_length,
        script_sig: script_sig.to_vec(),
        sequence,
    }))
}

fn parse_tx_output(input: &[u8]) -> IResult<&[u8], TxOutput> {
    let (input, value) = le_u64(input)?;
    let (input, script_pubkey_length) = le_u64(input)?;
    let (input, script_pubkey) = take(script_pubkey_length)(input)?;

    Ok((input, TxOutput {
        value,
        script_pubkey_length,
        script_pubkey: script_pubkey.to_vec(),
    }))
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    let (input, version) = le_u32(input)?;
    let (input, input_count) = le_u64(input)?;
    let (input, inputs) = count(parse_tx_input, input_count as usize)(input)?;
    let (input, output_count) = le_u64(input)?;
    let (input, outputs) = count(parse_tx_output, output_count as usize)(input)?;
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
            std::process::exit(1);
        }
    }
}