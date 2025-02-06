use nom::{
    bytes::complete::take,
    combinator::map,
    multi::length_data,
    number::complete::{le_u32, le_u64, u8},
    IResult,
};
use std::env;
use std::fs;

#[derive(Debug)]
struct TxIn {
    previous_output_hash: Vec<u8>,
    previous_output_index: u32,
    script_sig: Vec<u8>,
    sequence: u32,
}

#[derive(Debug)]
struct TxOut {
    value: u64,
    script_pub_key: Vec<u8>,
}

#[derive(Debug)]
struct Transaction {
    version: i32,
    inputs: Vec<TxIn>,
    outputs: Vec<TxOut>,
    lock_time: u32,
    segwit_marker: Option<u8>,
    segwit_flag: Option<u8>,
    witness_data: Vec<Vec<u8>>,
}

fn parse_varint(input: &[u8]) -> IResult<&[u8], usize> {
    let (input, first_byte) = u8(input)?;
    match first_byte {
        0xFD => map(take(2usize), |bytes: &[u8]| u16::from_le_bytes([bytes[0], bytes[1]]) as usize)(input),
        0xFE => map(take(4usize), |bytes: &[u8]| u32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]) as usize)(input),
        0xFF => map(take(8usize), |bytes: &[u8]| u64::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7]]) as usize)(input),
        _ => Ok((input, first_byte as usize)),
    }
}

fn parse_txin(input: &[u8]) -> IResult<&[u8], TxIn> {
    let (input, previous_output_hash) = take(32usize)(input)?;
    let (input, previous_output_index) = le_u32(input)?;
    let (input, script_sig) = length_data(parse_varint)(input)?;
    let (input, sequence) = le_u32(input)?;
    Ok((input, TxIn {
        previous_output_hash: previous_output_hash.to_vec(),
        previous_output_index,
        script_sig: script_sig.to_vec(),
        sequence,
    }))
}

fn parse_txout(input: &[u8]) -> IResult<&[u8], TxOut> {
    let (input, value) = le_u64(input)?;
    let (input, script_pub_key) = length_data(parse_varint)(input)?;
    Ok((input, TxOut {
        value,
        script_pub_key: script_pub_key.to_vec(),
    }))
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    let (input, version) = map(le_u32, |v| v as i32)(input)?;

    let (input, marker) = u8(input)?;
    let (mut input, has_segwit) = if marker == 0x00 {
        let (input, flag) = u8(input)?;
        if flag == 0x01 {
            (input, true)
        } else {
            return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag)));
        }
    } else {
        (input, false)
    };

    let (input, input_count) = parse_varint(input)?;
    let (input, inputs) = nom::multi::count(parse_txin, input_count)(input)?;

    let (input, output_count) = parse_varint(input)?;
    let (input, outputs) = nom::multi::count(parse_txout, output_count)(input)?;

    let (input, witness_data) = if has_segwit {
        let (input, witness_count) = parse_varint(input)?;
        let (input, witness_data) = nom::multi::count(length_data(parse_varint), witness_count)(input)?;
        (input, witness_data.into_iter().map(|w| w.to_vec()).collect())
    } else {
        (input, vec![])
    };

    let (input, lock_time) = le_u32(input)?;

    Ok((input, Transaction {
        version,
        inputs,
        outputs,
        lock_time,
        segwit_marker: if has_segwit { Some(marker) } else { None },
        segwit_flag: if has_segwit { Some(0x01) } else { None },
        witness_data,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let file_path = &args[1];
    let data = fs::read(file_path).expect("Failed to read file");

    match parse_transaction(&data) {
        Ok((_, transaction)) => println!("{:?}", transaction),
        Err(e) => eprintln!("Error parsing transaction: {:?}", e),
    }
}