use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::{count, many0},
    number::complete::{le_u32, le_u64, le_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::{env, fs, io::Read, path::Path};

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
    inputs: Vec<TxIn>,
    outputs: Vec<TxOut>,
    locktime: u32,
}

fn parse_outpoint(i: &[u8]) -> IResult<&[u8], OutPoint> {
    let (i, hash) = take(32usize)(i)?;
    let (i, index) = le_u32(i)?;
    Ok((i, OutPoint { hash: hash.to_vec(), index }))
}

fn parse_txin(i: &[u8]) -> IResult<&[u8], TxIn> {
    let (i, previous_output) = parse_outpoint(i)?;
    let (i, script_len) = map_res(le_u8, |len: u8| Ok(len as usize))(i)?;
    let (i, script_sig) = take(script_len)(i)?;
    let (i, sequence) = le_u32(i)?;
    Ok((i, TxIn {
        previous_output,
        script_sig: script_sig.to_vec(),
        sequence,
    }))
}

fn parse_txout(i: &[u8]) -> IResult<&[u8], TxOut> {
    let (i, value) = le_u64(i)?;
    let (i, script_len) = map_res(le_u8, |len: u8| Ok(len as usize))(i)?;
    let (i, script_pubkey) = take(script_len)(i)?;
    Ok((i, TxOut {
        value,
        script_pubkey: script_pubkey.to_vec(),
    }))
}

fn parse_transaction(i: &[u8]) -> IResult<&[u8], Transaction> {
    let (i, version) = le_u32(i)?;
    let (i, input_count) = map_res(le_u8, |count: u8| Ok(count as usize))(i)?;
    let (i, inputs) = count(parse_txin, input_count)(i)?;
    let (i, output_count) = map_res(le_u8, |count: u8| Ok(count as usize))(i)?;
    let (i, outputs) = count(parse_txout, output_count)(i)?;
    let (i, locktime) = le_u32(i)?;
    Ok((i, Transaction {
        version,
        inputs,
        outputs,
        locktime,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file_path>", args[0]);
        std::process::exit(1);
    }
    let path = Path::new(&args[1]);
    let mut file = fs::File::open(path).expect("File not found");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Error reading file");

    match parse_transaction(&buffer) {
        Ok((_remaining, transaction)) => {
            println!("Parsed Transaction: {:#?}", transaction);
        }
        Err(err) => {
            println!("Error parsing transaction: {:?}", err);
        }
    }
}