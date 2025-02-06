use std::fs::File;
use std::io::Read;
use std::env;
use nom::{
    bytes::complete::take,
    number::complete::{le_u32, le_u64, le_u8, le_u16},
    IResult,
    combinator::map,
    multi::length_data,
    sequence::tuple,
};

#[derive(Debug)]
struct TransactionInput {
    prev_tx_hash: [u8; 32],
    prev_tx_out_index: u32,
    script_sig: Vec<u8>,
    sequence: u32,
}

#[derive(Debug)]
struct TransactionOutput {
    value: u64,
    script_pub_key: Vec<u8>,
}

#[derive(Debug)]
struct BitcoinTransaction {
    version: u32,
    inputs: Vec<TransactionInput>,
    outputs: Vec<TransactionOutput>,
    locktime: u32,
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

fn parse_transaction_input(input: &[u8]) -> IResult<&[u8], TransactionInput> {
    let (input, (prev_tx_hash, prev_tx_out_index, script_sig, sequence)) = tuple((
        map(take(32usize), |b: &[u8]| {
            let mut arr = [0u8; 32];
            arr.copy_from_slice(b);
            arr
        }),
        le_u32,
        length_data(parse_varint),
        le_u32,
    ))(input)?;

    Ok((input, TransactionInput {
        prev_tx_hash,
        prev_tx_out_index,
        script_sig: script_sig.to_vec(),
        sequence,
    }))
}

fn parse_transaction_output(input: &[u8]) -> IResult<&[u8], TransactionOutput> {
    let (input, (value, script_pub_key)) = tuple((
        le_u64,
        length_data(parse_varint),
    ))(input)?;

    Ok((input, TransactionOutput {
        value,
        script_pub_key: script_pub_key.to_vec(),
    }))
}

fn parse_bitcoin_transaction(input: &[u8]) -> IResult<&[u8], BitcoinTransaction> {
    let (input, version) = le_u32(input)?;
    let (input, input_count) = parse_varint(input)?;
    let mut inputs = Vec::with_capacity(input_count as usize);
    let mut input_rem = input;
    for _ in 0..input_count {
        let (rem, input) = parse_transaction_input(input_rem)?;
        inputs.push(input);
        input_rem = rem;
    }

    let (input, output_count) = parse_varint(input_rem)?;
    let mut outputs = Vec::with_capacity(output_count as usize);
    let mut output_rem = input;
    for _ in 0..output_count {
        let (rem, output) = parse_transaction_output(output_rem)?;
        outputs.push(output);
        output_rem = rem;
    }

    let (input, locktime) = le_u32(output_rem)?;

    Ok((input, BitcoinTransaction {
        version,
        inputs,
        outputs,
        locktime,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <transaction_file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_bitcoin_transaction(&buffer) {
        Ok((_, transaction)) => println!("{:?}", transaction),
        Err(e) => eprintln!("Failed to parse transaction: {:?}", e),
    }
}