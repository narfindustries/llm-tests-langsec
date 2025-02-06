use nom::multi::count;
use nom::number::complete::{le_u32, le_u64};
use nom::IResult;
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct Transaction {
    version: u32,
    inputs: Vec<TxInput>,
    outputs: Vec<TxOutput>,
    locktime: u32,
}

#[derive(Debug)]
struct TxInput {
    prev_tx_hash: [u8; 32],
    prev_output_index: u32,
    script_sig: Vec<u8>,
    sequence: u32,
}

#[derive(Debug)]
struct TxOutput {
    value: u64,
    script_pubkey: Vec<u8>,
}

fn parse_varint(input: &[u8]) -> IResult<&[u8], u64> {
    let (input, first_byte) = nom::number::complete::le_u8(input)?;
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
            let (input, value) = nom::number::complete::le_u16(input)?;
            Ok((input, value as u64))
        }
        _ => Ok((input, first_byte as u64)),
    }
}

fn parse_script(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, script_len) = parse_varint(input)?;
    let (input, script) = nom::bytes::complete::take(script_len as usize)(input)?;
    Ok((input, script.to_vec()))
}

fn parse_tx_input(input: &[u8]) -> IResult<&[u8], TxInput> {
    let (input, prev_tx_hash) = nom::bytes::complete::take(32usize)(input)?;
    let (input, prev_output_index) = le_u32(input)?;
    let (input, script_sig) = parse_script(input)?;
    let (input, sequence) = le_u32(input)?;

    Ok((
        input,
        TxInput {
            prev_tx_hash: prev_tx_hash.try_into().unwrap(),
            prev_output_index,
            script_sig,
            sequence,
        },
    ))
}

fn parse_tx_output(input: &[u8]) -> IResult<&[u8], TxOutput> {
    let (input, value) = le_u64(input)?;
    let (input, script_pubkey) = parse_script(input)?;

    Ok((
        input,
        TxOutput {
            value,
            script_pubkey,
        },
    ))
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    let (input, version) = le_u32(input)?;
    let (input, input_count) = parse_varint(input)?;
    let (input, inputs) = count(parse_tx_input, input_count as usize)(input)?;
    let (input, output_count) = parse_varint(input)?;
    let (input, outputs) = count(parse_tx_output, output_count as usize)(input)?;
    let (input, locktime) = le_u32(input)?;

    Ok((
        input,
        Transaction {
            version,
            inputs,
            outputs,
            locktime,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <bitcoin_transaction_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)
        .expect("Failed to read file contents");

    match parse_transaction(&buffer) {
        Ok((remaining, transaction)) => {
            println!("Parsed Transaction: {:#?}", transaction);
            if !remaining.is_empty() {
                println!("Warning: {} bytes remaining", remaining.len());
            }
        }
        Err(e) => {
            eprintln!("Failed to parse transaction: {:?}", e);
            std::process::exit(1);
        }
    }
}