use nom::{
    bytes::complete::take,
    combinator::map,
    multi::count,
    number::complete::{le_u32, le_u64, le_u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct Transaction {
    version: u32,
    marker: Option<u8>,
    flag: Option<u8>,
    inputs: Vec<TxInput>,
    outputs: Vec<TxOutput>,
    witness_data: Option<Vec<Vec<Vec<u8>>>>,
    locktime: u32,
}

#[derive(Debug)]
struct TxInput {
    prev_tx_hash: [u8; 32],
    prev_output_index: u32,
    script: Vec<u8>,
    sequence: u32,
}

#[derive(Debug)]
struct TxOutput {
    value: u64,
    script: Vec<u8>,
}

fn parse_varint(input: &[u8]) -> IResult<&[u8], u64> {
    let (input, first_byte) = le_u8(input)?;
    match first_byte {
        0xff => {
            let (input, value) = le_u64(input)?;
            Ok((input, value))
        }
        0xfe => {
            let (input, value) = le_u32(input)?;
            Ok((input, value as u64))
        }
        0xfd => {
            let (input, value) = le_u16(input)?;
            Ok((input, value as u64))
        }
        _ => Ok((input, first_byte as u64)),
    }
}

fn le_u16(input: &[u8]) -> IResult<&[u8], u16> {
    map(take(2usize), |bytes: &[u8]| {
        ((bytes[1] as u16) << 8) | (bytes[0] as u16)
    })(input)
}

fn parse_tx_input(input: &[u8]) -> IResult<&[u8], TxInput> {
    let (input, prev_tx_hash) = map(take(32usize), |bytes: &[u8]| {
        let mut hash = [0u8; 32];
        hash.copy_from_slice(bytes);
        hash
    })(input)?;
    let (input, prev_output_index) = le_u32(input)?;
    let (input, script_len) = parse_varint(input)?;
    let (input, script) = map(take(script_len as usize), |s: &[u8]| s.to_vec())(input)?;
    let (input, sequence) = le_u32(input)?;

    Ok((
        input,
        TxInput {
            prev_tx_hash,
            prev_output_index,
            script,
            sequence,
        },
    ))
}

fn parse_tx_output(input: &[u8]) -> IResult<&[u8], TxOutput> {
    let (input, value) = le_u64(input)?;
    let (input, script_len) = parse_varint(input)?;
    let (input, script) = map(take(script_len as usize), |s: &[u8]| s.to_vec())(input)?;

    Ok((input, TxOutput { value, script }))
}

fn parse_witness_stack(input: &[u8]) -> IResult<&[u8], Vec<Vec<u8>>> {
    let (input, stack_items) = parse_varint(input)?;
    let mut remaining = input;
    let mut items = Vec::new();

    for _ in 0..stack_items {
        let (input, item_len) = parse_varint(remaining)?;
        let (input, item) = map(take(item_len as usize), |s: &[u8]| s.to_vec())(input)?;
        items.push(item);
        remaining = input;
    }

    Ok((remaining, items))
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    let (input, version) = le_u32(input)?;
    
    let (input, maybe_marker) = le_u8(input)?;
    let (input, marker, flag) = if maybe_marker == 0 {
        let (input, flag_byte) = le_u8(input)?;
        if flag_byte == 1 {
            (input, Some(0), Some(1))
        } else {
            let (_, input_count) = parse_varint(&[maybe_marker, flag_byte][..])?;
            (input, None, None)
        }
    } else {
        let (_, input_count) = parse_varint(&[maybe_marker][..])?;
        (input, None, None)
    };

    let (input, input_count) = parse_varint(input)?;
    let (input, inputs) = count(parse_tx_input, input_count as usize)(input)?;

    let (input, output_count) = parse_varint(input)?;
    let (input, outputs) = count(parse_tx_output, output_count as usize)(input)?;

    let (input, witness_data) = if marker.is_some() {
        let (input, witness_stacks) = count(parse_witness_stack, inputs.len())(input)?;
        (input, Some(witness_stacks))
    } else {
        (input, None)
    };

    let (input, locktime) = le_u32(input)?;

    Ok((
        input,
        Transaction {
            version,
            marker,
            flag,
            inputs,
            outputs,
            witness_data,
            locktime,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <transaction_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_transaction(&buffer) {
        Ok((remaining, transaction)) => {
            println!("Parsed transaction: {:#?}", transaction);
            println!("Remaining bytes: {} bytes", remaining.len());
        }
        Err(e) => {
            eprintln!("Failed to parse transaction: {:?}", e);
            std::process::exit(1);
        }
    }
}