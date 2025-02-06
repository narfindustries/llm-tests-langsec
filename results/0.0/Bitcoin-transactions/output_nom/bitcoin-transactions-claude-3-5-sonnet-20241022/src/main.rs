use nom::{
    bytes::complete::{take},
    number::complete::{le_u8, le_u16, le_u32, le_u64},
    IResult,
    multi::count,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct Transaction {
    version: u32,
    inputs: Vec<TxInput>,
    outputs: Vec<TxOutput>,
    lock_time: u32,
}

#[derive(Debug)]
struct TxInput {
    prev_tx_hash: [u8; 32],
    prev_tx_index: u32,
    script_sig: Vec<u8>,
    sequence: u32,
}

#[derive(Debug)]
struct TxOutput {
    value: u64,
    script_pubkey: Vec<u8>,
}

fn parse_varint(input: &[u8]) -> IResult<&[u8], u64> {
    let (input, first_byte) = le_u8(input)?;
    match first_byte {
        0..=0xfc => Ok((input, first_byte as u64)),
        0xfd => {
            let (input, value) = le_u16(input)?;
            Ok((input, value as u64))
        },
        0xfe => {
            let (input, value) = le_u32(input)?;
            Ok((input, value as u64))
        },
        0xff => {
            let (input, value) = le_u64(input)?;
            Ok((input, value))
        },
    }
}

fn parse_tx_input(input: &[u8]) -> IResult<&[u8], TxInput> {
    let (input, prev_tx_hash_slice) = take(32usize)(input)?;
    let mut prev_tx_hash = [0u8; 32];
    prev_tx_hash.copy_from_slice(prev_tx_hash_slice);
    
    let (input, prev_tx_index) = le_u32(input)?;
    let (input, script_len) = parse_varint(input)?;
    let (input, script_sig) = take(script_len as usize)(input)?;
    let (input, sequence) = le_u32(input)?;

    Ok((input, TxInput {
        prev_tx_hash,
        prev_tx_index,
        script_sig: script_sig.to_vec(),
        sequence,
    }))
}

fn parse_tx_output(input: &[u8]) -> IResult<&[u8], TxOutput> {
    let (input, value) = le_u64(input)?;
    let (input, script_len) = parse_varint(input)?;
    let (input, script_pubkey) = take(script_len as usize)(input)?;

    Ok((input, TxOutput {
        value,
        script_pubkey: script_pubkey.to_vec(),
    }))
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    let (input, version) = le_u32(input)?;
    let (input, input_count) = parse_varint(input)?;
    let (input, inputs) = count(parse_tx_input, input_count as usize)(input)?;
    let (input, output_count) = parse_varint(input)?;
    let (input, outputs) = count(parse_tx_output, output_count as usize)(input)?;
    let (input, lock_time) = le_u32(input)?;

    Ok((input, Transaction {
        version,
        inputs,
        outputs,
        lock_time,
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
        Ok((remaining, transaction)) => {
            println!("Successfully parsed transaction:");
            println!("Version: {}", transaction.version);
            println!("Number of inputs: {}", transaction.inputs.len());
            for (i, input) in transaction.inputs.iter().enumerate() {
                println!("Input {}:", i);
                println!("  Previous tx hash: {:?}", input.prev_tx_hash);
                println!("  Previous tx index: {}", input.prev_tx_index);
                println!("  ScriptSig length: {}", input.script_sig.len());
                println!("  Sequence: {}", input.sequence);
            }
            println!("Number of outputs: {}", transaction.outputs.len());
            for (i, output) in transaction.outputs.iter().enumerate() {
                println!("Output {}:", i);
                println!("  Value: {} satoshis", output.value);
                println!("  ScriptPubKey length: {}", output.script_pubkey.len());
            }
            println!("Lock time: {}", transaction.lock_time);
            println!("Remaining unparsed bytes: {}", remaining.len());
        },
        Err(e) => {
            eprintln!("Error parsing transaction: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}