use nom::{
    bytes::complete::take,
    multi::count,
    number::complete::{be_u32, be_u64},
    IResult,
};
use std::fs;

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

fn parse_tx_out_point(input: &[u8]) -> IResult<&[u8], TxOutPoint> {
    let (rest, hash) = take(32usize)(input)?;
    let (rest, index) = be_u32(rest)?;
    Ok((rest, TxOutPoint { hash: hash.try_into().unwrap(), index }))
}

fn parse_varint(input: &[u8]) -> IResult<&[u8], u64> {
    let (rest, first_byte) = take(1usize)(input)?;
    let first_byte = first_byte[0];
    match first_byte {
        0xfd => {
            let (rest, num) = be_u16(rest)?;
            Ok((rest, num as u64))
        }
        0xfe => {
            let (rest, num) = be_u32(rest)?;
            Ok((rest, num as u64))
        }
        0xff => {
            let (rest, num) = be_u64(rest)?;
            Ok((rest, num))
        }
        _ => Ok((rest, first_byte as u64)),
    }
}

fn be_u16(input: &[u8]) -> IResult<&[u8], u16> {
    let (rest, bytes) = take(2usize)(input)?;
    let num = u16::from_be_bytes(bytes.try_into().unwrap());
    Ok((rest, num))
}

fn parse_tx_in(input: &[u8]) -> IResult<&[u8], TxIn> {
    let (rest, prev_out) = parse_tx_out_point(input)?;
    let (rest, script_sig_len) = parse_varint(rest)?;
    let (rest, script_sig) = take(script_sig_len as usize)(rest)?;
    let (rest, sequence) = be_u32(rest)?;
    Ok((
        rest,
        TxIn {
            prev_out,
            script_sig: script_sig.to_vec(),
            sequence,
        },
    ))
}

fn parse_tx_out(input: &[u8]) -> IResult<&[u8], TxOut> {
    let (rest, value) = be_u64(input)?;
    let (rest, script_pubkey_len) = parse_varint(rest)?;
    let (rest, script_pubkey) = take(script_pubkey_len as usize)(rest)?;
    Ok((
        rest,
        TxOut {
            value,
            script_pubkey: script_pubkey.to_vec(),
        },
    ))
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    let (rest, version) = be_u32(input)?;
    let (rest, num_inputs) = parse_varint(rest)?;
    let (rest, inputs) = count(num_inputs as usize, parse_tx_in)(rest)?;
    let (rest, num_outputs) = parse_varint(rest)?;
    let (rest, outputs) = count(num_outputs as usize, parse_tx_out)(rest)?;
    let (rest, locktime) = be_u32(rest)?;
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
        println!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let contents = fs::read(filename).expect("Something went wrong reading the file");

    match parse_transaction(&contents) {
        Ok((_, transaction)) => println!("{:#?}", transaction),
        Err(e) => println!("Error parsing transaction: {:?}", e),
    }
}
