use nom::{
    bytes::complete::{take, take_while},
    combinator::{map, map_res},
    multi::count,
    number::complete::{le_u32, le_u64, le_u8},
    sequence::tuple,
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::env;

#[derive(Debug)]
struct OutPoint {
    hash: Vec<u8>,
    index: u32,
}

#[derive(Debug)]
struct TxIn {
    previous_output: OutPoint,
    script_sig: Vec<u8>,
    sequence: u32,
}

#[derive(Debug)]
struct TxOut {
    value: u64,
    script_pubkey: Vec<u8>,
}

#[derive(Debug)]
struct Transaction {
    version: u32,
    tx_in_count: u8,
    tx_ins: Vec<TxIn>,
    tx_out_count: u8,
    tx_outs: Vec<TxOut>,
    lock_time: u32,
}

fn parse_outpoint(input: &[u8]) -> IResult<&[u8], OutPoint> {
    let (input, hash) = take(32usize)(input)?;
    let (input, index) = le_u32(input)?;
    Ok((input, OutPoint { hash: hash.to_vec(), index }))
}

fn parse_txin(input: &[u8]) -> IResult<&[u8], TxIn> {
    let (input, previous_output) = parse_outpoint(input)?;
    let (input, script_len) = le_u8(input)?;
    let (input, script_sig) = take(script_len)(input)?;
    let (input, sequence) = le_u32(input)?;
    Ok((input, TxIn {
        previous_output,
        script_sig: script_sig.to_vec(),
        sequence,
    }))
}

fn parse_txout(input: &[u8]) -> IResult<&[u8], TxOut> {
    let (input, value) = le_u64(input)?;
    let (input, script_len) = le_u8(input)?;
    let (input, script_pubkey) = take(script_len)(input)?;
    Ok((input, TxOut {
        value,
        script_pubkey: script_pubkey.to_vec(),
    }))
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    let (input, version) = le_u32(input)?;
    let (input, tx_in_count) = le_u8(input)?;
    let (input, tx_ins) = count(parse_txin, tx_in_count as usize)(input)?;
    let (input, tx_out_count) = le_u8(input)?;
    let (input, tx_outs) = count(parse_txout, tx_out_count as usize)(input)?;
    let (input, lock_time) = le_u32(input)?;
    Ok((input, Transaction {
        version,
        tx_in_count,
        tx_ins,
        tx_out_count,
        tx_outs,
        lock_time,
    }))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        return Err(io::Error::new(io::ErrorKind::InvalidInput, "Usage: bitcoin_parser <file_path>"));
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_transaction(&buffer) {
        Ok((_, transaction)) => println!("{:#?}", transaction),
        Err(e) => println!("Failed to parse transaction: {:?}", e),
    }

    Ok(())
}