use nom::{
    be_u8, be_u16, be_u32, be_u64,
    bytes::complete::take,
    combinator::{map, opt, verify},
    multi::length_count,
    sequence::{tuple, preceded},
    IResult,
};
use std::fs::read;
use std::env;


#[derive(Debug)]
struct Transaction {
    version: u32,
    inputs: Vec<TransactionInput>,
    outputs: Vec<TransactionOutput>,
    lock_time: u32,
}

#[derive(Debug)]
struct TransactionInput {
    previous_output: PreviousOutput,
    script_sig: Vec<u8>,
    sequence: u32,
}

#[derive(Debug)]
struct PreviousOutput {
    hash: [u8; 32],
    index: u32,
}

#[derive(Debug)]
struct TransactionOutput {
    value: u64,
    script_pubkey: Vec<u8>,
}


fn parse_varint(input: &[u8]) -> IResult<&[u8], u64> {
    let (remaining, first) = be_u8(input)?;
    match first {
        0xfd => map(be_u16, |x| x as u64)(remaining),
        0xfe => map(be_u32, |x| x as u64)(remaining),
        0xff => map(be_u64, |x| x)(remaining),
        _ => Ok((remaining, first as u64)),
    }
}

fn parse_previous_output(input: &[u8]) -> IResult<&[u8], PreviousOutput> {
    let (input, hash) = take(32usize)(input)?;
    let (input, index) = be_u32(input)?;
    Ok((input, PreviousOutput { hash: hash.try_into().unwrap(), index }))
}

fn parse_transaction_input(input: &[u8]) -> IResult<&[u8], TransactionInput> {
    let (input, previous_output) = parse_previous_output(input)?;
    let (input, script_sig_len) = parse_varint(input)?;
    let (input, script_sig) = take(script_sig_len as usize)(input)?;
    let (input, sequence) = be_u32(input)?;
    Ok((
        input,
        TransactionInput {
            previous_output,
            script_sig: script_sig.to_vec(),
            sequence,
        },
    ))
}

fn parse_transaction_output(input: &[u8]) -> IResult<&[u8], TransactionOutput> {
    let (input, value) = be_u64(input)?;
    let (input, script_pubkey_len) = parse_varint(input)?;
    let (input, script_pubkey) = take(script_pubkey_len as usize)(input)?;
    Ok((
        input,
        TransactionOutput {
            value,
            script_pubkey: script_pubkey.to_vec(),
        },
    ))
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    let (input, version) = be_u32(input)?;
    let (input, num_inputs) = parse_varint(input)?;
    let (input, inputs) = length_count(num_inputs as usize, parse_transaction_input)(input)?;
    let (input, num_outputs) = parse_varint(input)?;
    let (input, outputs) = length_count(num_outputs as usize, parse_transaction_output)(input)?;
    let (input, lock_time) = be_u32(input)?;
    Ok((
        input,
        Transaction {
            version,
            inputs,
            outputs,
            lock_time,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let data = read(filename).expect("Failed to read file");

    match parse_transaction(&data) {
        Ok((_, transaction)) => println!("{:#?}", transaction),
        Err(e) => eprintln!("Parsing error: {:?}", e),
    }
}
