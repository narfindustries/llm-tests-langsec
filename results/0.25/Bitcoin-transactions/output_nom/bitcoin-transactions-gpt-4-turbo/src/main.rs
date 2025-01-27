use nom::{
    bytes::complete::{take, take_while_m_n},
    combinator::{map, map_res},
    multi::count,
    number::complete::{le_u32, le_u64, le_u8},
    sequence::tuple,
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::env;

#[derive(Debug)]
struct TxInput {
    previous_output: (u32, u32),
    script_length: u64,
    script_sig: Vec<u8>,
    sequence: u32,
}

#[derive(Debug)]
struct TxOutput {
    value: u64,
    pk_script_length: u64,
    pk_script: Vec<u8>,
}

#[derive(Debug)]
struct Transaction {
    version: u32,
    input_count: u64,
    inputs: Vec<TxInput>,
    output_count: u64,
    outputs: Vec<TxOutput>,
    lock_time: u32,
}

fn parse_var_int(input: &[u8]) -> IResult<&[u8], u64> {
    let (input, first_byte) = le_u8(input)?;
    match first_byte {
        0xfd => {
            let (input, value) = map_res(take(2u8), |bytes: &[u8]| {
                bytes.try_into().map(u16::from_le_bytes)
            })(input)?;
            Ok((input, value as u64))
        }
        0xfe => {
            let (input, value) = map_res(take(4u8), |bytes: &[u8]| {
                bytes.try_into().map(u32::from_le_bytes)
            })(input)?;
            Ok((input, value as u64))
        }
        0xff => {
            let (input, value) = le_u64(input)?;
            Ok((input, value))
        }
        _ => Ok((input, first_byte as u64)),
    }
}

fn parse_input(input: &[u8]) -> IResult<&[u8], TxInput> {
    let (input, (txid, vout, script_length)) = tuple((take(32u8), le_u32, parse_var_int))(input)?;
    let (input, script_sig) = take(script_length)(input)?;
    let (input, sequence) = le_u32(input)?;
    Ok((
        input,
        TxInput {
            previous_output: (txid, vout),
            script_length,
            script_sig: script_sig.to_vec(),
            sequence,
        },
    ))
}

fn parse_output(input: &[u8]) -> IResult<&[u8], TxOutput> {
    let (input, (value, pk_script_length)) = tuple((le_u64, parse_var_int))(input)?;
    let (input, pk_script) = take(pk_script_length)(input)?;
    Ok((
        input,
        TxOutput {
            value,
            pk_script_length,
            pk_script: pk_script.to_vec(),
        },
    ))
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    let (input, version) = le_u32(input)?;
    let (input, input_count) = parse_var_int(input)?;
    let (input, inputs) = count(parse_input, input_count as usize)(input)?;
    let (input, output_count) = parse_var_int(input)?;
    let (input, outputs) = count(parse_output, output_count as usize)(input)?;
    let (input, lock_time) = le_u32(input)?;
    Ok((
        input,
        Transaction {
            version,
            input_count,
            inputs,
            output_count,
            outputs,
            lock_time,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        std::process::exit(1);
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