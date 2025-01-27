use nom::{
    bytes::complete::{tag, take},
    combinator::map_res,
    multi::length_data,
    number::complete::{le_u32, le_u64},
    sequence::tuple,
    IResult,
};
use std::{env, fs, io::Read};

#[derive(Debug)]
struct Transaction {
    version: u32,
    inputs: Vec<TxIn>,
    outputs: Vec<TxOut>,
    lock_time: u32,
}

#[derive(Debug)]
struct TxIn {
    previous_output: OutPoint,
    script_sig: Vec<u8>,
    sequence: u32,
}

#[derive(Debug)]
struct OutPoint {
    hash: [u8; 32],
    index: u32,
}

#[derive(Debug)]
struct TxOut {
    value: u64,
    script_pubkey: Vec<u8>,
}

fn parse_outpoint(input: &[u8]) -> IResult<&[u8], OutPoint> {
    let (input, hash) = take(32usize)(input)?;
    let (input, index) = le_u32(input)?;
    Ok((input, OutPoint { hash: hash.try_into().unwrap(), index }))
}

fn parse_txin(input: &[u8]) -> IResult<&[u8], TxIn> {
    let (input, previous_output) = parse_outpoint(input)?;
    let (input, script_sig) = length_data(map_res(le_u32, |len| len.try_into()))(input)?;
    let (input, sequence) = le_u32(input)?;
    Ok((input, TxIn { previous_output, script_sig: script_sig.to_vec(), sequence }))
}

fn parse_txout(input: &[u8]) -> IResult<&[u8], TxOut> {
    let (input, value) = le_u64(input)?;
    let (input, script_pubkey) = length_data(map_res(le_u32, |len| len.try_into()))(input)?;
    Ok((input, TxOut { value, script_pubkey: script_pubkey.to_vec() }))
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    let (input, version) = le_u32(input)?;
    let (input, inputs) = length_data(map_res(le_u32, |len| len.try_into()))(input)?;
    let (input, outputs) = length_data(map_res(le_u32, |len| len.try_into()))(input)?;
    let (input, lock_time) = le_u32(input)?;

    let (_, inputs) = nom::multi::many1(parse_txin)(inputs)?;
    let (_, outputs) = nom::multi::many1(parse_txout)(outputs)?;

    Ok((input, Transaction { version, inputs, outputs, lock_time }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <binary file>", args[0]);
        return;
    }

    let mut file = fs::File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_transaction(&buffer) {
        Ok((_, transaction)) => println!("{:#?}", transaction),
        Err(e) => eprintln!("Failed to parse transaction: {:?}", e),
    }
}