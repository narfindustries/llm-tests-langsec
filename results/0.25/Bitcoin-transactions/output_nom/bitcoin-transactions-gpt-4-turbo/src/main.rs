use nom::{
    bytes::complete::take,
    combinator::{map_res, map},
    multi::count,
    number::complete::{le_u32, le_u64},
    sequence::tuple,
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::env;

#[derive(Debug)]
struct Transaction {
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
    script_pub_key: Vec<u8>,
}

fn parse_var_int(input: &[u8]) -> IResult<&[u8], u64> {
    let first_byte = input[0];
    match first_byte {
        0xfd => {
            let (input, value) = map_res(take(2usize), |bytes: &[u8]| {
                bytes.try_into().map(u16::from_le_bytes)
            })(input)?;
            Ok((input, value as u64))
        }
        0xfe => {
            let (input, value) = map_res(take(4usize), |bytes: &[u8]| {
                bytes.try_into().map(u32::from_le_bytes)
            })(input)?;
            Ok((input, value as u64))
        }
        0xff => {
            let (input, value) = map_res(take(8usize), |bytes: &[u8]| {
                bytes.try_into().map(u64::from_le_bytes)
            })(input)?;
            Ok((input, value))
        }
        _ => Ok((input, first_byte as u64)),
    }
}

fn parse_input(input: &[u8]) -> IResult<&[u8], TxInput> {
    let (input, (previous_output, script_len)) = tuple((
        parse_out_point,
        parse_var_int,
    ))(input)?;
    let (input, script_sig) = take(script_len)(input)?;
    let (input, sequence) = le_u32(input)?;
    Ok((
        input,
        TxInput {
            previous_output,
            script_sig: script_sig.to_vec(),
            sequence,
        },
    ))
}

fn parse_out_point(input: &[u8]) -> IResult<&[u8], OutPoint> {
    let (input, (hash, index)) = tuple((take(32usize), le_u32))(input)?;
    let hash_array = <[u8; 32]>::try_from(hash).unwrap();
    Ok((input, OutPoint { hash: hash_array, index }))
}

fn parse_output(input: &[u8]) -> IResult<&[u8], TxOutput> {
    let (input, (value, script_len)) = tuple((
        le_u64,
        parse_var_int,
    ))(input)?;
    let (input, script_pub_key) = take(script_len)(input)?;
    Ok((
        input,
        TxOutput {
            value,
            script_pub_key: script_pub_key.to_vec(),
        },
    ))
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    let (input, (version, input_count)) = tuple((le_u32, parse_var_int))(input)?;
    let (input, inputs) = count(parse_input, input_count as usize)(input)?;
    let (input, output_count) = parse_var_int(input)?;
    let (input, outputs) = count(parse_output, output_count as usize)(input)?;
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
        eprintln!("Usage: {} <file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_transaction(&buffer) {
        Ok((_, tx)) => println!("{:#?}", tx),
        Err(e) => eprintln!("Failed to parse transaction: {:?}", e),
    }

    Ok(())
}