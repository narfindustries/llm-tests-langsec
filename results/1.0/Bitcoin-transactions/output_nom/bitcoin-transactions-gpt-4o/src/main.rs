use nom::{
    bytes::complete::{take, take_while},
    combinator::{map, map_res, opt},
    multi::{count, length_data},
    number::complete::{le_i64, le_u32, le_u64, le_u8},
    IResult,
};
use std::env;
use std::fs;
use std::path::Path;

// Bitcoin VarInt parser
fn parse_varint(input: &[u8]) -> IResult<&[u8], u64> {
    let (input, first_byte) = le_u8(input)?;
    match first_byte {
        0xFD => map(le_u16, u64::from)(input),
        0xFE => map(le_u32, u64::from)(input),
        0xFF => le_u64(input),
        _ => Ok((input, u64::from(first_byte))),
    }
}

// Bitcoin script parser
fn parse_script(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    length_data(parse_varint)(input).map(|(i, data)| (i, data.to_vec()))
}

// Bitcoin transaction input parser
fn parse_transaction_input(input: &[u8]) -> IResult<&[u8], TransactionInput> {
    let (input, previous_output) = parse_outpoint(input)?;
    let (input, script) = parse_script(input)?;
    let (input, sequence) = le_u32(input)?;
    Ok((input, TransactionInput {
        previous_output,
        script,
        sequence,
    }))
}

// Bitcoin transaction outpoint parser
fn parse_outpoint(input: &[u8]) -> IResult<&[u8], OutPoint> {
    let (input, hash) = take(32_usize)(input)?;
    let (input, index) = le_u32(input)?;
    Ok((input, OutPoint {
        hash: hash.to_vec(),
        index,
    }))
}

// Bitcoin transaction output parser
fn parse_transaction_output(input: &[u8]) -> IResult<&[u8], TransactionOutput> {
    let (input, value) = le_i64(input)?;
    let (input, script_pubkey) = parse_script(input)?;
    Ok((input, TransactionOutput {
        value,
        script_pubkey,
    }))
}

// Bitcoin transaction parser
fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    let (input, version) = le_u32(input)?;
    let (input, has_witness) = is_witness_marker(input)?;
    let (input, input_count) = parse_varint(input)?;
    let (input, inputs) = count(parse_transaction_input, input_count as usize)(input)?;
    let (input, output_count) = parse_varint(input)?;
    let (input, outputs) = count(parse_transaction_output, output_count as usize)(input)?;
    let (input, witnesses) = if has_witness {
        let witnesses_parser = count(length_data(parse_varint), input_count as usize);
        let (input, witnesses) = witnesses_parser(input)?;
        (input, Some(witnesses.iter().map(|w| w.to_vec()).collect()))
    } else {
        (input, None)
    };
    let (input, lock_time) = le_u32(input)?;

    Ok((input, Transaction {
        version,
        inputs,
        outputs,
        witnesses,
        lock_time,
    }))
}

// Check if input data is witness marker
fn is_witness_marker(input: &[u8]) -> IResult<&[u8], bool> {
    let (input, marker) = le_u8(input)?;
    let (input, flag) = le_u8(input)?;
    if marker == 0 && flag != 0 {
        Ok((input, true))
    } else {
        Ok((input, false))
    }
}

// Data structure definitions
#[derive(Debug)]
struct Transaction {
    version: u32,
    inputs: Vec<TransactionInput>,
    outputs: Vec<TransactionOutput>,
    witnesses: Option<Vec<Vec<u8>>>,
    lock_time: u32,
}

#[derive(Debug)]
struct TransactionInput {
    previous_output: OutPoint,
    script: Vec<u8>,
    sequence: u32,
}

#[derive(Debug)]
struct OutPoint {
    hash: Vec<u8>,
    index: u32,
}

#[derive(Debug)]
struct TransactionOutput {
    value: i64,
    script_pubkey: Vec<u8>,
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_binary_file>", args[0]);
        return;
    }

    let input_path = &args[1];
    let input = fs::read(input_path).expect("Failed to read input file");
    
    match parse_transaction(&input) {
        Ok((_, transaction)) => {
            println!("{:?}", transaction);
        }
        Err(e) => {
            eprintln!("Failed to parse transaction: {:?}", e);
        }
    }
}