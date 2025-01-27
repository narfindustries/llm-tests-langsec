use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_opt},
    error::ParseError,
    multi::{count, length_data, length_value, many0},
    number::complete::{le_i32, le_u32, le_u64, le_u8, le_u16},
    sequence::{preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

// Bitcoin VarInt
fn varint(input: &[u8]) -> IResult<&[u8], u64> {
    let (input, first_byte) = le_u8(input)?;
    match first_byte {
        0xFD => map(le_u16, u64::from)(input),
        0xFE => map(le_u32, u64::from)(input),
        0xFF => le_u64(input),
        _ => Ok((input, u64::from(first_byte))),
    }
}

// Bitcoin Script
fn script(input: &[u8]) -> IResult<&[u8], &[u8]> {
    length_data(varint)(input)
}

// Bitcoin Transaction Input
fn tx_input(input: &[u8]) -> IResult<&[u8], TxInput> {
    let (input, (prev_txid, prev_vout, script_sig, sequence)) = tuple((
        take(32usize), // Previous transaction hash
        le_u32,        // Previous transaction output index
        script,        // ScriptSig
        le_u32,        // Sequence
    ))(input)?;
    Ok((
        input,
        TxInput {
            prev_txid: prev_txid.to_vec(),
            prev_vout,
            script_sig: script_sig.to_vec(),
            sequence,
        },
    ))
}

// Bitcoin Transaction Output
fn tx_output(input: &[u8]) -> IResult<&[u8], TxOutput> {
    let (input, (value, script_pubkey)) = tuple((le_u64, script))(input)?;
    Ok((
        input,
        TxOutput {
            value,
            script_pubkey: script_pubkey.to_vec(),
        },
    ))
}

// Bitcoin Transaction
fn transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    let (input, (version, inputs, outputs, locktime)) = tuple((
        le_i32,                                  // Version
        length_value(varint, many0(tx_input)),   // Inputs
        length_value(varint, many0(tx_output)),  // Outputs
        le_u32,                                  // Locktime
    ))(input)?;
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

// Transaction Input Struct
#[derive(Debug)]
struct TxInput {
    prev_txid: Vec<u8>,
    prev_vout: u32,
    script_sig: Vec<u8>,
    sequence: u32,
}

// Transaction Output Struct
#[derive(Debug)]
struct TxOutput {
    value: u64,
    script_pubkey: Vec<u8>,
}

// Transaction Struct
#[derive(Debug)]
struct Transaction {
    version: i32,
    inputs: Vec<TxInput>,
    outputs: Vec<TxOutput>,
    locktime: u32,
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match transaction(&buffer) {
        Ok((_, tx)) => println!("{:?}", tx),
        Err(e) => eprintln!("Failed to parse transaction: {:?}", e),
    }
}