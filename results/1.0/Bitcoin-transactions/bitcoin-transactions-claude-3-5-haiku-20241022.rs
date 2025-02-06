use std::fs::File;
use std::io::Read;
use std::env;
use nom::{
    multi::{count},
    number::complete::{le_u32, le_u64, le_u8},
    bytes::complete::{take},
    sequence::{tuple},
    combinator::{map, map_opt},
    IResult,
};

#[derive(Debug)]
struct BitcoinTransaction {
    version: u32,
    inputs: Vec<TransactionInput>,
    outputs: Vec<TransactionOutput>,
    locktime: u32,
}

#[derive(Debug)]
struct TransactionInput {
    previous_tx_hash: [u8; 32],
    previous_output_index: u32,
    script_sig_length: u8,
    script_sig: Vec<u8>,
    sequence: u32,
}

#[derive(Debug)]
struct TransactionOutput {
    amount: u64,
    script_pubkey_length: u8,
    script_pubkey: Vec<u8>,
}

fn parse_transaction_input(input: &[u8]) -> IResult<&[u8], TransactionInput> {
    map(
        tuple((
            take(32usize),
            le_u32,
            le_u8,
            map_opt(|i| take_bytes(i), |bytes: Vec<u8>| Some(bytes)),
            le_u32
        )),
        |(hash, prev_index, script_sig_len, script_sig, sequence)| TransactionInput {
            previous_tx_hash: hash.try_into().unwrap(),
            previous_output_index: prev_index,
            script_sig_length: script_sig_len,
            script_sig,
            sequence,
        }
    )(input)
}

fn take_bytes(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (remaining, length) = le_u8(input)?;
    map(take(length as usize), |bytes: &[u8]| bytes.to_vec())(remaining)
}

fn parse_transaction_output(input: &[u8]) -> IResult<&[u8], TransactionOutput> {
    map(
        tuple((
            le_u64,
            le_u8,
            map_opt(|i| take_bytes(i), |bytes: Vec<u8>| Some(bytes))
        )),
        |(amount, script_pubkey_len, script_pubkey)| TransactionOutput {
            amount,
            script_pubkey_length: script_pubkey_len,
            script_pubkey,
        }
    )(input)
}

fn parse_bitcoin_transaction(input: &[u8]) -> IResult<&[u8], BitcoinTransaction> {
    let (input, version) = le_u32(input)?;
    let (input, input_count) = le_u8(input)?;
    let (input, inputs) = count(parse_transaction_input, input_count as usize)(input)?;
    let (input, output_count) = le_u8(input)?;
    let (input, outputs) = count(parse_transaction_output, output_count as usize)(input)?;
    let (input, locktime) = le_u32(input)?;

    Ok((input, BitcoinTransaction {
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

    match parse_bitcoin_transaction(&buffer) {
        Ok((_, transaction)) => {
            println!("Parsed Transaction: {:?}", transaction);
            Ok(())
        },
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1)
        }
    }
}