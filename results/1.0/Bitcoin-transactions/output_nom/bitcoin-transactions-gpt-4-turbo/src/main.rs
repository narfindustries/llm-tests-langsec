use nom::{
    bytes::complete::take,
    combinator::{map_res, map_parser},
    number::complete::{le_i32, le_i64, le_u32},
    sequence::{tuple},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug)]
struct TxInput {
    previous_tx_hash: Vec<u8>,
    previous_tx_out_index: u32,
    script_sig: Vec<u8>,
    sequence: u32,
}

#[derive(Debug)]
struct TxOutput {
    value: i64,
    script_pub_key: Vec<u8>,
}

#[derive(Debug)]
struct Transaction {
    version: i32,
    inputs: Vec<TxInput>,
    outputs: Vec<TxOutput>,
    lock_time: u32,
}

fn parse_var_int(input: &[u8]) -> IResult<&[u8], u64> {
    let (input, first_byte) = take(1usize)(input)?;
    match first_byte[0] {
        0xff => map_res(take(8usize), |bytes: &[u8]| Ok::<u64, std::array::TryFromSliceError>(u64::from_le_bytes(bytes.try_into().unwrap())))(input),
        0xfe => map_res(take(4usize), |bytes: &[u8]| Ok::<u64, std::array::TryFromSliceError>(u32::from_le_bytes(bytes.try_into().unwrap()) as u64))(input),
        0xfd => map_res(take(2usize), |bytes: &[u8]| Ok::<u64, std::array::TryFromSliceError>(u16::from_le_bytes(bytes.try_into().unwrap()) as u64))(input),
        x => Ok((input, x as u64)),
    }
}

fn parse_hex(input: &[u8], count: usize) -> IResult<&[u8], Vec<u8>> {
    take(count)(input).map(|(i, result)| (i, result.to_vec()))
}

fn parse_inputs(input: &[u8], count: usize) -> IResult<&[u8], Vec<TxInput>> {
    let mut inputs = Vec::new();
    let mut ix = input;
    for _ in 0..count {
        let (next_input, (previous_tx_hash, previous_tx_out_index, script_length, script_sig, sequence)) =
            tuple((map_parser(take(32usize), parse_hex), le_u32, parse_var_int, map_parser(parse_var_int, |i| parse_hex(i, script_length as usize)), le_u32))(ix)?;
        inputs.push(TxInput {
            previous_tx_hash,
            previous_tx_out_index,
            script_sig,
            sequence,
        });
        ix = next_input;
    }
    Ok((ix, inputs))
}

fn parse_outputs(input: &[u8], count: usize) -> IResult<&[u8], Vec<TxOutput>> {
    let mut outputs = Vec::new();
    let mut ix = input;
    for _ in 0..count {
        let (next_input, (value, script_length, script_pub_key)) = tuple((le_i64, parse_var_int, map_parser(parse_var_int, |i| parse_hex(i, script_length as usize))))(ix)?;
        outputs.push(TxOutput {
            value,
            script_pub_key,
        });
        ix = next_input;
    }
    Ok((ix, outputs))
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    let (input, version) = le_i32(input)?;
    let (input, input_count) = parse_var_int(input)?;
    let (input, inputs) = parse_inputs(input, input_count as usize)?;
    let (input, output_count) = parse_var_int(input)?;
    let (input, outputs) = parse_outputs(input, output_count as usize)?;
    let (input, lock_time) = le_u32(input)?;
    Ok((input, Transaction {
        version,
        inputs,
        outputs,
        lock_time,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file_path>", args[0]);
        return;
    }

    let file_path = &args[1];
    let mut file = File::open(file_path).expect("Unable to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Error reading file");

    match parse_transaction(&buffer) {
        Ok((_, transaction)) => println!("{:?}", transaction),
        Err(e) => eprintln!("Error parsing transaction: {:?}", e),
    }
}