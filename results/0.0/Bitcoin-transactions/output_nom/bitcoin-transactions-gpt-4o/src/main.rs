use std::fs::File;
use std::io::Read;
use std::env;
use nom::{
    IResult,
    bytes::complete::{take, take_while},
    number::complete::{le_u32, le_u64, le_u8, le_u16},
    combinator::map,
    multi::length_data,
    sequence::tuple,
};

#[derive(Debug)]
struct BitcoinTransaction {
    version: u32,
    inputs: Vec<TxInput>,
    outputs: Vec<TxOutput>,
    lock_time: u32,
}

#[derive(Debug)]
struct TxInput {
    previous_output: OutPoint,
    script_sig: Vec<u8>,
    sequence: u32,
}

#[derive(Debug)]
struct OutPoint {
    hash: [u8; 32],
    index: u32,
}

#[derive(Debug)]
struct TxOutput {
    value: u64,
    script_pubkey: Vec<u8>,
}

fn parse_varint(input: &[u8]) -> IResult<&[u8], u64> {
    let (input, first_byte) = le_u8(input)?;
    match first_byte {
        0xFD => map(le_u16, u64::from)(input),
        0xFE => map(le_u32, u64::from)(input),
        0xFF => le_u64(input),
        _ => Ok((input, first_byte as u64)),
    }
}

fn parse_outpoint(input: &[u8]) -> IResult<&[u8], OutPoint> {
    let (input, (hash, index)) = tuple((take(32usize), le_u32))(input)?;
    let mut hash_array = [0u8; 32];
    hash_array.copy_from_slice(hash);
    Ok((input, OutPoint { hash: hash_array, index }))
}

fn parse_tx_input(input: &[u8]) -> IResult<&[u8], TxInput> {
    let (input, previous_output) = parse_outpoint(input)?;
    let (input, script_sig) = length_data(parse_varint)(input)?;
    let (input, sequence) = le_u32(input)?;
    Ok((input, TxInput {
        previous_output,
        script_sig: script_sig.to_vec(),
        sequence,
    }))
}

fn parse_tx_output(input: &[u8]) -> IResult<&[u8], TxOutput> {
    let (input, value) = le_u64(input)?;
    let (input, script_pubkey) = length_data(parse_varint)(input)?;
    Ok((input, TxOutput {
        value,
        script_pubkey: script_pubkey.to_vec(),
    }))
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], BitcoinTransaction> {
    let (input, version) = le_u32(input)?;
    let (input, input_count) = parse_varint(input)?;
    let (input, inputs) = nom::multi::count(parse_tx_input, input_count as usize)(input)?;
    let (input, output_count) = parse_varint(input)?;
    let (input, outputs) = nom::multi::count(parse_tx_output, output_count as usize)(input)?;
    let (input, lock_time) = le_u32(input)?;
    Ok((input, BitcoinTransaction {
        version,
        inputs,
        outputs,
        lock_time,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_transaction(&buffer) {
        Ok((_, transaction)) => println!("{:?}", transaction),
        Err(e) => eprintln!("Failed to parse transaction: {:?}", e),
    }
}