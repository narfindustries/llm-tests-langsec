use nom::{
    be_u8, be_u16, be_u32, be_u64,
    bytes::complete::take,
    combinator::map,
    multi::length_count,
    sequence::{tuple, preceded},
    IResult,
};
use std::fs::read;
use std::env;

#[derive(Debug)]
struct Transaction {
    version: u32,
    inputs: Vec<TxIn>,
    outputs: Vec<TxOut>,
    lock_time: u32,
}

#[derive(Debug)]
struct TxIn {
    prev_hash: [u8; 32],
    prev_index: u32,
    script_sig: Vec<u8>,
    sequence: u32,
}

#[derive(Debug)]
struct TxOut {
    value: u64,
    script_pubkey: Vec<u8>,
}


fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    let (input, version) = be_u32(input)?;

    let (input, num_inputs) = be_u32(input)?;
    let (input, inputs) = length_count(num_inputs as usize, parse_txin)(input)?;

    let (input, num_outputs) = be_u32(input)?;
    let (input, outputs) = length_count(num_outputs as usize, parse_txout)(input)?;

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

fn parse_txin(input: &[u8]) -> IResult<&[u8], TxIn> {
    let (input, prev_hash) = take(32usize)(input)?;
    let (input, prev_index) = be_u32(input)?;
    let (input, script_len) = be_u16(input)?;
    let (input, script_sig) = take(script_len as usize)(input)?;
    let (input, sequence) = be_u32(input)?;

    Ok((
        input,
        TxIn {
            prev_hash: prev_hash.try_into().unwrap(),
            prev_index,
            script_sig: script_sig.to_vec(),
            sequence,
        },
    ))
}

fn parse_txout(input: &[u8]) -> IResult<&[u8], TxOut> {
    let (input, value) = be_u64(input)?;
    let (input, script_len) = be_u16(input)?;
    let (input, script_pubkey) = take(script_len as usize)(input)?;

    Ok((
        input,
        TxOut {
            value,
            script_pubkey: script_pubkey.to_vec(),
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
    let data = match read(filename) {
        Ok(data) => data,
        Err(e) => {
            println!("Error reading file: {}", e);
            return;
        }
    };

    match parse_transaction(&data) {
        Ok((_, transaction)) => println!("{:#?}", transaction),
        Err(e) => println!("Error parsing transaction: {:?}", e),
    }
}
