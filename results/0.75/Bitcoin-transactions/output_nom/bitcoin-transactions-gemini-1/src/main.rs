use nom::{
    be_u8, be_u16, be_u32, be_u64,
    bytes::complete::take,
    combinator::{map, opt, verify},
    multi::length_count,
    number::complete::be_i64,
    sequence::{pair, tuple},
    IResult,
};
use std::fs::read;
use std::env;

#[derive(Debug)]
struct TransactionInput {
    previous_transaction_hash: [u8; 32],
    previous_output_index: u32,
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


fn parse_transaction_input(input: &[u8]) -> IResult<&[u8], TransactionInput> {
    let (input, previous_transaction_hash) = take(32u8)(input)?;
    let (input, previous_output_index) = be_u32(input)?;
    let (input, script_sig_len) = be_u16(input)?;
    let (input, script_sig) = take(script_sig_len as usize)(input)?;
    let (input, sequence) = be_u32(input)?;
    Ok((input, TransactionInput {
        previous_transaction_hash: previous_transaction_hash.try_into().unwrap(),
        previous_output_index,
        script_sig: script_sig.to_vec(),
        sequence,
    }))
}

fn parse_transaction_output(input: &[u8]) -> IResult<&[u8], TransactionOutput> {
    let (input, value) = be_u64(input)?;
    let (input, script_pubkey_len) = be_u16(input)?;
    let (input, script_pubkey) = take(script_pubkey_len as usize)(input)?;
    Ok((input, TransactionOutput {
        value,
        script_pubkey: script_pubkey.to_vec(),
    }))
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    let (input, version) = be_u32(input)?;
    let (input, num_inputs) = be_u16(input)?;
    let (input, inputs) = length_count(num_inputs as usize, parse_transaction_input)(input)?;
    let (input, num_outputs) = be_u16(input)?;
    let (input, outputs) = length_count(num_outputs as usize, parse_transaction_output)(input)?;
    let (input, locktime) = be_u32(input)?;
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
        println!("Usage: {} <binary_file>", args[0]);
        return;
    }
    let filename = &args[1];
    match read(filename) {
        Ok(buffer) => {
            match parse_transaction(&buffer) {
                Ok((_, transaction)) => println!("{:#?}", transaction),
                Err(e) => println!("Parsing error: {:?}", e),
            }
        }
        Err(e) => println!("Error reading file: {}", e),
    }
}
