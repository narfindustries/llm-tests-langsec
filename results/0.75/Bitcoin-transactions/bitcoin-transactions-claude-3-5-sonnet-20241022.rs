use nom::{
    bytes::complete::{tag, take},
    combinator::map,
    multi::count,
    number::complete::{le_u16, le_u32, le_u64, le_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs;
use std::vec::Vec;

#[derive(Debug)]
struct TxIn {
    previous_output: OutPoint,
    script_length: u64,
    signature_script: Vec<u8>,
    sequence: u32,
}

#[derive(Debug)]
struct TxOut {
    value: u64,
    pk_script_length: u64,
    pk_script: Vec<u8>,
}

#[derive(Debug)]
struct OutPoint {
    hash: Vec<u8>,
    index: u32,
}

#[derive(Debug)]
struct Transaction {
    version: u32,
    flag: Option<u16>,
    tx_in_count: u64,
    tx_in: Vec<TxIn>,
    tx_out_count: u64,
    tx_out: Vec<TxOut>,
    lock_time: u32,
}

fn parse_varint(input: &[u8]) -> IResult<&[u8], u64> {
    let (input, first_byte) = le_u8(input)?;
    match first_byte {
        0xff => le_u64(input),
        0xfe => map(le_u32, |x| x as u64)(input),
        0xfd => map(le_u16, |x| x as u64)(input),
        _ => Ok((input, first_byte as u64)),
    }
}

fn parse_outpoint(input: &[u8]) -> IResult<&[u8], OutPoint> {
    let (input, (hash, index)) = tuple((take(32usize), le_u32))(input)?;
    Ok((
        input,
        OutPoint {
            hash: hash.to_vec(),
            index,
        },
    ))
}

fn parse_txin(input: &[u8]) -> IResult<&[u8], TxIn> {
    let (input, previous_output) = parse_outpoint(input)?;
    let (input, script_length) = parse_varint(input)?;
    let (input, signature_script) = take(script_length as usize)(input)?;
    let (input, sequence) = le_u32(input)?;
    Ok((
        input,
        TxIn {
            previous_output,
            script_length,
            signature_script: signature_script.to_vec(),
            sequence,
        },
    ))
}

fn parse_txout(input: &[u8]) -> IResult<&[u8], TxOut> {
    let (input, value) = le_u64(input)?;
    let (input, pk_script_length) = parse_varint(input)?;
    let (input, pk_script) = take(pk_script_length as usize)(input)?;
    Ok((
        input,
        TxOut {
            value,
            pk_script_length,
            pk_script: pk_script.to_vec(),
        },
    ))
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    let (input, version) = le_u32(input)?;
    let (input, flag) = if input.len() >= 2 && input[0] == 0x00 && input[1] == 0x01 {
        let (input, _) = tag(&[0x00, 0x01])(input)?;
        (input, Some(0x0001))
    } else {
        (input, None)
    };
    let (input, tx_in_count) = parse_varint(input)?;
    let (input, tx_in) = count(parse_txin, tx_in_count as usize)(input)?;
    let (input, tx_out_count) = parse_varint(input)?;
    let (input, tx_out) = count(parse_txout, tx_out_count as usize)(input)?;
    let (input, lock_time) = le_u32(input)?;
    Ok((
        input,
        Transaction {
            version,
            flag,
            tx_in_count,
            tx_in,
            tx_out_count,
            tx_out,
            lock_time,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <transaction_file>", args[0]);
        std::process::exit(1);
    }

    let data = fs::read(&args[1]).expect("Failed to read file");
    match parse_transaction(&data) {
        Ok((remaining, transaction)) => {
            println!("Parsed transaction: {:#?}", transaction);
            println!("Remaining bytes: {} bytes", remaining.len());
        }
        Err(e) => eprintln!("Failed to parse transaction: {:?}", e),
    }
}