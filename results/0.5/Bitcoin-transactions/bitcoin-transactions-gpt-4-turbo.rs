use nom::{
    bytes::complete::{take},
    number::complete::{le_u32, le_u64},
    IResult,
    multi::count,
    sequence::tuple,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct Transaction {
    version: u32,
    inputs: Vec<TxInput>,
    outputs: Vec<TxOutput>,
    lock_time: u32,
}

#[derive(Debug)]
struct TxInput {
    previous_output_hash: Vec<u8>,
    previous_output_index: u32,
    script_sig: Vec<u8>,
    sequence: u32,
}

#[derive(Debug)]
struct TxOutput {
    value: u64,
    script_pub_key: Vec<u8>,
}

fn parse_varint(input: &[u8]) -> IResult<&[u8], usize> {
    let first_byte = input[0];
    match first_byte {
        0xfd => {
            let (input, value) = nom::number::complete::le_u16(&input[1..])?;
            Ok((input, value as usize))
        },
        0xfe => {
            let (input, value) = nom::number::complete::le_u32(&input[1..])?;
            Ok((input, value as usize))
        },
        0xff => {
            let (input, value) = nom::number::complete::le_u64(&input[1..])?;
            Ok((input, value as usize))
        },
        _ => Ok((&input[1..], first_byte as usize)),
    }
}

fn parse_input(input: &[u8]) -> IResult<&[u8], TxInput> {
    let (input, (previous_output_hash, previous_output_index)) = tuple((
        take(32usize),
        le_u32,
    ))(input)?;
    let (input, script_length) = parse_varint(input)?;
    let (input, script_sig) = take(script_length)(input)?;
    let (input, sequence) = le_u32(input)?;

    Ok((
        input,
        TxInput {
            previous_output_hash: previous_output_hash.to_vec(),
            previous_output_index,
            script_sig: script_sig.to_vec(),
            sequence,
        },
    ))
}

fn parse_output(input: &[u8]) -> IResult<&[u8], TxOutput> {
    let (input, value) = le_u64(input)?;
    let (input, script_length) = parse_varint(input)?;
    let (input, script_pub_key) = take(script_length)(input)?;

    Ok((
        input,
        TxOutput {
            value,
            script_pub_key: script_pub_key.to_vec(),
        },
    ))
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    let (input, version) = le_u32(input)?;
    let (input, input_count) = parse_varint(input)?;
    let (input, inputs) = count(parse_input, input_count)(input)?;
    let (input, output_count) = parse_varint(input)?;
    let (input, outputs) = count(parse_output, output_count)(input)?;
    let (input, lock_time) = le_u32(input)?;

    Ok((
        input,
        Transaction {
            version,
            inputs,
            outputs,
            lock_time,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <file_path>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_transaction(&buffer) {
        Ok((_, tx)) => println!("{:#?}", tx),
        Err(e) => println!("Failed to parse transaction: {:?}", e),
    }

    Ok(())
}