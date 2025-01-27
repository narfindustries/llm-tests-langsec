use nom::branch::alt;
use nom::bytes::complete::{be_u32, be_u64, be_u8, take};
use nom::combinator::{flat_map, map};
use nom::multi::many0;
use nom::sequence::{preceded, tuple};
use nom::IResult;
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug, PartialEq)]
enum TxInType {
    CoinBase,
    Regular,
}

#[derive(Debug, PartialEq)]
struct TxIn {
    tx_id: Vec<u8>,
    vout: u32,
    script_sig: Vec<u8>,
    sequence: u32,
    tx_in_type: TxInType,
}

fn parse_tx_in(input: &[u8]) -> IResult<&[u8], TxIn> {
    map(
        tuple((
            take(32),
            be_u32,
            preceded(be_u8, take),
            be_u32,
        )),
        |(tx_id, vout, script_sig, sequence)| TxIn {
            tx_id,
            vout,
            script_sig,
            sequence,
            tx_in_type: if tx_id == [0; 32] {
                TxInType::CoinBase
            } else {
                TxInType::Regular
            },
        },
    )(input)
}

#[derive(Debug, PartialEq)]
struct TxOut {
    value: u64,
    script_pub_key: Vec<u8>,
}

fn parse_tx_out(input: &[u8]) -> IResult<&[u8], TxOut> {
    map(
        tuple((be_u64, preceded(be_u8, take))),
        |(value, script_pub_key)| TxOut { value, script_pub_key },
    )(input)
}

#[derive(Debug, PartialEq)]
struct Transaction {
    version: u32,
    tx_ins: Vec<TxIn>,
    tx_outs: Vec<TxOut>,
    lock_time: u32,
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    map(
        tuple((
            be_u32,
            preceded(be_u8, many0(parse_tx_in)),
            preceded(be_u8, many0(parse_tx_out)),
            be_u32,
        )),
        |(version, tx_ins, tx_outs, lock_time)| Transaction {
            version,
            tx_ins,
            tx_outs,
            lock_time,
        },
    )(input)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut data = Vec::new();
    file.read_to_end(&mut data).expect("Failed to read file");

    match parse_transaction(&data) {
        Ok((_, transaction)) => println!("{:?}", transaction),
        Err(err) => eprintln!("Error parsing transaction: {:?}", err),
    }
}