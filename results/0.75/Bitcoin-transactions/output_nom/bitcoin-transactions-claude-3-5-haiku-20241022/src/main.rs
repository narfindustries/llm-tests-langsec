use nom::{
    bits::{complete::take_bits, streaming::take as take_bits_stream},
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{count, many0},
    number::complete::{le_u16, le_u32, le_u64},
    sequence::{pair, tuple},
    IResult, Parser,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct TransactionInput {
    prev_tx_hash: [u8; 32],
    prev_output_index: u32,
    script_sig_length: u64,
    script_sig: Vec<u8>,
    sequence: u32,
}

#[derive(Debug)]
struct TransactionOutput {
    amount: u64,
    script_pubkey_length: u64,
    script_pubkey: Vec<u8>,
}

#[derive(Debug)]
struct Transaction {
    version: u32,
    input_count: u64,
    inputs: Vec<TransactionInput>,
    output_count: u64,
    outputs: Vec<TransactionOutput>,
    locktime: u32,
    witness: Option<Vec<Vec<u8>>>,
}

fn parse_transaction_input(input: &[u8]) -> IResult<&[u8], TransactionInput> {
    let (input, prev_tx_hash) = take(32usize)(input)?;
    let (input, prev_output_index) = le_u32(input)?;
    let (input, script_sig_length) = nom::combinator::map(nom::number::complete::var_int, |x| x as u64)(input)?;
    let (input, script_sig) = take(script_sig_length)(input)?;
    let (input, sequence) = le_u32(input)?;

    Ok((input, TransactionInput {
        prev_tx_hash: prev_tx_hash.try_into().unwrap(),
        prev_output_index,
        script_sig_length,
        script_sig: script_sig.to_vec(),
        sequence,
    }))
}

fn parse_transaction_output(input: &[u8]) -> IResult<&[u8], TransactionOutput> {
    let (input, amount) = le_u64(input)?;
    let (input, script_pubkey_length) = nom::combinator::map(nom::number::complete::var_int, |x| x as u64)(input)?;
    let (input, script_pubkey) = take(script_pubkey_length)(input)?;

    Ok((input, TransactionOutput {
        amount,
        script_pubkey_length,
        script_pubkey: script_pubkey.to_vec(),
    }))
}

fn parse_witness(input: &[u8]) -> IResult<&[u8], Vec<Vec<u8>>> {
    let (input, witness_items) = many0(|i| {
        let (i, witness_length) = nom::number::complete::var_int(i)?;
        let (i, witness_data) = take(witness_length)(i)?;
        Ok((i, witness_data.to_vec()))
    })(input)?;

    Ok((input, witness_items))
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    let (input, version) = le_u32(input)?;
    let (input, is_witness) = take_bits(1u8)(input)?;
    let (input, input_count) = nom::number::complete::var_int(input)?;
    let (input, inputs) = count(parse_transaction_input, input_count as usize)(input)?;
    let (input, output_count) = nom::number::complete::var_int(input)?;
    let (input, outputs) = count(parse_transaction_output, output_count as usize)(input)?;

    let (input, witness) = if is_witness == 1 {
        let (input, witness_data) = parse_witness(input)?;
        (input, Some(witness_data))
    } else {
        (input, None)
    };

    let (input, locktime) = le_u32(input)?;

    Ok((input, Transaction {
        version,
        input_count,
        inputs,
        output_count,
        outputs,
        locktime,
        witness,
    }))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <transaction_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_transaction(&buffer) {
        Ok((_, transaction)) => {
            println!("Parsed Transaction: {:?}", transaction);
            Ok(())
        }
        Err(e) => {
            eprintln!("Failed to parse transaction: {:?}", e);
            std::process::exit(1);
        }
    }
}