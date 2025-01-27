use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::{length_data, many0},
    number::complete::{le_u32, le_u64},
    sequence::{preceded, tuple},
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
    script: Vec<u8>,
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
    script: Vec<u8>,
}

fn parse_outpoint(input: &[u8]) -> IResult<&[u8], OutPoint> {
    map(
        tuple((take(32usize), le_u32)),
        |(hash, index)| OutPoint {
            hash: hash.try_into().unwrap(),
            index,
        },
    )(input)
}

fn parse_txin(input: &[u8]) -> IResult<&[u8], TxIn> {
    map(
        tuple((parse_outpoint, length_data(le_u32), le_u32)),
        |(previous_output, script, sequence)| TxIn {
            previous_output,
            script: script.to_vec(),
            sequence,
        },
    )(input)
}

fn parse_txout(input: &[u8]) -> IResult<&[u8], TxOut> {
    map(
        tuple((le_u64, length_data(le_u32))),
        |(value, script)| TxOut {
            value,
            script: script.to_vec(),
        },
    )(input)
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    map(
        tuple((
            le_u32,
            many0(parse_txin),
            many0(parse_txout),
            le_u32,
        )),
        |(version, inputs, outputs, lock_time)| Transaction {
            version,
            inputs,
            outputs,
            lock_time,
        },
    )(input)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
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