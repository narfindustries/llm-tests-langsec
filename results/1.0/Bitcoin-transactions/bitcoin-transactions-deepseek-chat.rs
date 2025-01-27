use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::length_count,
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
            hash: *array_ref!(hash, 0, 32),
            index,
        },
    )(input)
}

fn parse_txin(input: &[u8]) -> IResult<&[u8], TxIn> {
    map(
        tuple((parse_outpoint, parse_varint_vec, le_u32)),
        |(previous_output, script, sequence)| TxIn {
            previous_output,
            script,
            sequence,
        },
    )(input)
}

fn parse_txout(input: &[u8]) -> IResult<&[u8], TxOut> {
    map(
        tuple((le_u64, parse_varint_vec)),
        |(value, script)| TxOut { value, script },
    )(input)
}

fn parse_varint(input: &[u8]) -> IResult<&[u8], usize> {
    map_res(take(1usize), |b: &[u8]| {
        let b = b[0];
        match b {
            0xFD => map_res(take(2usize), |v: &[u8]| Ok(u16::from_le_bytes(*array_ref!(v, 0, 2)) as usize))(input),
            0xFE => map_res(take(4usize), |v: &[u8]| Ok(u32::from_le_bytes(*array_ref!(v, 0, 4)) as usize))(input),
            0xFF => map_res(take(8usize), |v: &[u8]| Ok(u64::from_le_bytes(*array_ref!(v, 0, 8)) as usize))(input),
            _ => Ok(b as usize),
        }
    })(input)
}

fn parse_varint_vec(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    map(parse_varint, |len| input[..len].to_vec())(input)
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    map(
        tuple((le_u32, length_count(parse_varint, parse_txin), length_count(parse_varint, parse_txout), le_u32)),
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