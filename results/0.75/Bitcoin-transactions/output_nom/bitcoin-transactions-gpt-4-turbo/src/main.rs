use nom::{
    bytes::complete::take,
    number::complete::{le_u16, le_u32, le_u64},
    sequence::tuple,
    IResult,
    multi::count,
    combinator::map,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct TxInput {
    prev_tx_hash: Vec<u8>,
    prev_tx_index: u32,
    script_sig: Vec<u8>,
    sequence: u32,
}

#[derive(Debug)]
struct TxOutput {
    value: u64,
    script_pub_key: Vec<u8>,
}

#[derive(Debug)]
struct BitcoinTransaction {
    version: u32,
    inputs: Vec<TxInput>,
    outputs: Vec<TxOutput>,
    lock_time: u32,
}

fn parse_varint(input: &[u8]) -> IResult<&[u8], u64> {
    let first_byte = input[0];
    if first_byte < 0xfd {
        map(take(1usize), |bytes: &[u8]| bytes[0] as u64)(input)
    } else if first_byte == 0xfd {
        map(tuple((take(1usize), le_u16)), |(_, num)| num as u64)(input)
    } else if first_byte == 0xfe {
        map(tuple((take(1usize), le_u32)), |(_, num)| num as u64)(input)
    } else {
        map(tuple((take(1usize), le_u64)), |(_, num)| num)(input)
    }
}

fn parse_input(input: &[u8]) -> IResult<&[u8], TxInput> {
    let (input, (prev_tx_hash, prev_tx_index, script_length)) = tuple((
        take(32usize),
        le_u32,
        parse_varint,
    ))(input)?;
    let (input, script_sig) = take(script_length as usize)(input)?;
    let (input, sequence) = le_u32(input)?;
    Ok((
        input,
        TxInput {
            prev_tx_hash: prev_tx_hash.to_vec(),
            prev_tx_index,
            script_sig: script_sig.to_vec(),
            sequence,
        },
    ))
}

fn parse_output(input: &[u8]) -> IResult<&[u8], TxOutput> {
    let (input, (value, script_length)) = tuple((
        le_u64,
        parse_varint,
    ))(input)?;
    let (input, script_pub_key) = take(script_length as usize)(input)?;
    Ok((
        input,
        TxOutput {
            value,
            script_pub_key: script_pub_key.to_vec(),
        },
    ))
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], BitcoinTransaction> {
    let (input, version) = le_u32(input)?;
    let (input, input_count) = parse_varint(input)?;
    let (input, inputs) = count(parse_input, input_count as usize)(input)?;
    let (input, output_count) = parse_varint(input)?;
    let (input, outputs) = count(parse_output, output_count as usize)(input)?;
    let (input, lock_time) = le_u32(input)?;
    Ok((
        input,
        BitcoinTransaction {
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
        eprintln!("Usage: {} <file_path>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_transaction(&buffer) {
        Ok((_, tx)) => println!("{:?}", tx),
        Err(e) => eprintln!("Failed to parse transaction: {:?}", e),
    }

    Ok(())
}