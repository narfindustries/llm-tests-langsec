use nom::{
    bytes::complete::take,
    combinator::map,
    multi::count,
    number::complete::{le_i32, le_u32, le_u64},
    sequence::tuple,
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::env;

#[derive(Debug)]
struct Transaction {
    version: i32,
    inputs: Vec<TxInput>,
    outputs: Vec<TxOutput>,
    lock_time: u32,
}

#[derive(Debug)]
struct TxInput {
    previous_output: TxOutPoint,
    script_sig: Vec<u8>,
    sequence: u32,
}

#[derive(Debug)]
struct TxOutPoint {
    hash: Vec<u8>,
    index: u32,
}

#[derive(Debug)]
struct TxOutput {
    value: u64,
    script_pub_key: Vec<u8>,
}

fn parse_varint(input: &[u8]) -> IResult<&[u8], (u64, &[u8])> {
    let first_byte = input[0];
    match first_byte {
        0xfd => map(take(2usize), |bytes: &[u8]| (u64::from_le_bytes([bytes[0], bytes[1], 0, 0, 0, 0, 0, 0]), &input[3..]))(input),
        0xfe => map(take(4usize), |bytes: &[u8]| (u64::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3], 0, 0, 0, 0]), &input[5..]))(input),
        0xff => map(take(8usize), |bytes: &[u8]| (u64::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7]]), &input[9..]))(input),
        _ => Ok((&input[1..], (first_byte as u64, &input[1..])),
    }
}

fn parse_input(input: &[u8]) -> IResult<&[u8], TxInput> {
    let (input, previous_output) = parse_outpoint(input)?;
    let (input, (script_length, input)) = parse_varint(input)?;
    let (input, script_sig) = take(script_length)(input)?;
    let (input, sequence) = le_u32(input)?;
    Ok((input, TxInput {
        previous_output,
        script_sig: script_sig.to_vec(),
        sequence,
    }))
}

fn parse_outpoint(input: &[u8]) -> IResult<&[u8], TxOutPoint> {
    let (input, hash) = take(32usize)(input)?;
    let (input, index) = le_u32(input)?;
    Ok((input, TxOutPoint {
        hash: hash.to_vec(),
        index,
    }))
}

fn parse_output(input: &[u8]) -> IResult<&[u8], TxOutput> {
    let (input, value) = le_u64(input)?;
    let (input, (script_length, input)) = parse_varint(input)?;
    let (input, script_pub_key) = take(script_length)(input)?;
    Ok((input, TxOutput {
        value,
        script_pub_key: script_pub_key.to_vec(),
    }))
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    let (input, version) = le_i32(input)?;
    let (input, (input_count, input)) = parse_varint(input)?;
    let (input, inputs) = count(parse_input, input_count as usize)(input)?;
    let (input, (output_count, input)) = parse_varint(input)?;
    let (input, outputs) = count(parse_output, output_count as usize)(input)?;
    let (input, lock_time) = le_u32(input)?;
    Ok((input, Transaction {
        version,
        inputs,
        outputs,
        lock_time,
    }))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file_path>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_transaction(&buffer) {
        Ok((_, transaction)) => println!("{:#?}", transaction),
        Err(e) => eprintln!("Failed to parse transaction: {:?}", e),
    }

    Ok(())
}