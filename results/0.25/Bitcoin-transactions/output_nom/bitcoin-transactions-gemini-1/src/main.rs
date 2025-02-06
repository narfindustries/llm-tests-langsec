use nom::{
    bytes::complete::take,
    combinator::map_res,
    multi::length_count,
    number::complete::{be_u32, be_u64},
    sequence::{pair, tuple},
    IResult,
};
use std::fs::read;
use std::path::Path;

#[derive(Debug)]
struct Transaction {
    version: u32,
    inputs: Vec<TxIn>,
    outputs: Vec<TxOut>,
    locktime: u32,
}

#[derive(Debug)]
struct TxIn {
    prev_out: TxOutPoint,
    script_sig: Vec<u8>,
    sequence: u32,
}

#[derive(Debug)]
struct TxOutPoint {
    hash: [u8; 32],
    index: u32,
}

#[derive(Debug)]
struct TxOut {
    value: u64,
    script_pubkey: Vec<u8>,
}

fn read_varint(input: &[u8]) -> IResult<&[u8], u64> {
    let (rest, value) = be_u32(input)?;
    Ok((rest, value as u64))
}

fn read_bytes(input: &[u8], len: usize) -> IResult<&[u8], Vec<u8>> {
    let (rest, bytes) = take(len)(input)?;
    Ok((rest, bytes.to_vec()))
}

fn read_varbytes(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (rest, len) = read_varint(input)?;
    read_bytes(rest, len as usize)
}

fn parse_txin(input: &[u8]) -> IResult<&[u8], TxIn> {
    let (rest, (prev_out, script_sig, sequence)) = tuple((
        parse_txoutpoint,
        read_varbytes,
        be_u32,
    ))(input)?;
    Ok((rest, TxIn { prev_out, script_sig, sequence }))
}

fn parse_txout(input: &[u8]) -> IResult<&[u8], TxOut> {
    let (rest, (value, script_pubkey)) = pair(be_u64, read_varbytes)(input)?;
    Ok((rest, TxOut { value, script_pubkey }))
}

fn parse_txoutpoint(input: &[u8]) -> IResult<&[u8], TxOutPoint> {
    let (rest, (hash, index)) = tuple((
        map_res(take(32usize), |hash: &[u8]| {
            Ok::<[u8; 32], nom::Err<nom::error::Error<&[u8]>>>(hash.try_into().unwrap())
        }),
        be_u32,
    ))(input)?;
    Ok((rest, TxOutPoint { hash, index }))
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    let (rest, (version, _num_inputs, inputs, _num_outputs, outputs, locktime)) = tuple((
        be_u32,
        read_varint,
        length_count(read_varint, parse_txin),
        read_varint,
        length_count(read_varint, parse_txout),
        be_u32,
    ))(input)?;
    Ok((
        rest,
        Transaction {
            version,
            inputs,
            outputs,
            locktime,
        },
    ))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let bytes = match read(path) {
        Ok(bytes) => bytes,
        Err(e) => {
            eprintln!("Error reading file: {}", e);
            std::process::exit(1);
        }
    };

    match parse_transaction(&bytes) {
        Ok((rest, transaction)) => {
            println!("Parsed transaction: {:?}", transaction);
            if !rest.is_empty() {
                println!("Remaining bytes: {:?}", rest);
            }
        }
        Err(e) => {
            eprintln!("Error parsing transaction: {:?}", e);
            std::process::exit(1);
        }
    }
}
