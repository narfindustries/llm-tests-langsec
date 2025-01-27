use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{count, many0, many_m_n},
    number::complete::{le_u16, le_u32, le_u64},
    sequence::{preceded, tuple},
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
    map(
        tuple((
            take(32usize),
            le_u32,
            le_u64,
            |script_sig_length| take(script_sig_length as usize),
            le_u32,
        )),
        |(prev_tx_hash, prev_output_index, script_sig_length, script_sig, sequence)| TxInput {
            prev_tx_hash: prev_tx_hash.try_into().unwrap(),
            prev_output_index,
            script_sig_length,
            script_sig: script_sig.to_vec(),
            sequence,
        },
    )(input)
}

fn parse_tx_output(input: &[u8]) -> IResult<&[u8], TxOutput> {
    map(
        tuple((
            le_u64,
            le_u64,
            |script_pubkey_length| take(script_pubkey_length as usize),
        )),
        |(value, script_pubkey_length, script_pubkey)| TxOutput {
            value,
            script_pubkey_length,
            script_pubkey: script_pubkey.to_vec(),
        },
    )(input)
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    map(
        tuple((
            le_u32,
            le_u64,
            |input_count| many_m_n(input_count as usize, input_count as usize, parse_tx_input),
            le_u64,
            |output_count| many_m_n(output_count as usize, output_count as usize, parse_tx_output),
            le_u32,
        )),
        |(version, input_count, inputs, output_count, outputs, locktime)| Transaction {
            version,
            input_count,
            inputs,
            output_count,
            outputs,
            locktime,
        },
    )(input)
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