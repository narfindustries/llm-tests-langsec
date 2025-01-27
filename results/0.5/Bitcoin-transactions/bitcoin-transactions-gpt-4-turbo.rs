use nom::{
    bytes::complete::take,
    number::complete::{le_u32, le_u64, le_u8},
    sequence::tuple,
    IResult,
};
use std::{env, fs::File, io::Read, path::Path};

#[derive(Debug)]
struct TxIn {
    previous_output: (u32, u32), // (hash, index)
    script_sig: Vec<u8>,
    sequence: u32,
}

#[derive(Debug)]
struct TxOut {
    value: u64,
    script_pubkey: Vec<u8>,
}

#[derive(Debug)]
struct BitcoinTransaction {
    version: u32,
    inputs: Vec<TxIn>,
    outputs: Vec<TxOut>,
    lock_time: u32,
}

fn parse_varint(input: &[u8]) -> IResult<&[u8], u64> {
    let first_byte = input[0];
    match first_byte {
        0xff => {
            let (input, res) = le_u64(&input[1..])?;
            Ok((input, res))
        }
        0xfe => {
            let (input, res) = le_u32(&input[1..])?;
            Ok((input, res as u64))
        }
        0xfd => {
            let (input, res) = nom::number::complete::le_u16(&input[1..])?;
            Ok((input, res as u64))
        }
        _ => Ok((&input[1..], first_byte as u64)),
    }
}

fn parse_input(input: &[u8]) -> IResult<&[u8], TxIn> {
    let (input, (hash, index, script_len)) =
        tuple((take(32u8), le_u32, parse_varint))(input)?;
    let (input, script_sig) = take(script_len)(input)?;
    let (input, sequence) = le_u32(input)?;
    Ok((
        input,
        TxIn {
            previous_output: (u32::from_le_bytes(hash.try_into().unwrap()), index),
            script_sig: script_sig.to_vec(),
            sequence,
        },
    ))
}

fn parse_output(input: &[u8]) -> IResult<&[u8], TxOut> {
    let (input, value) = le_u64(input)?;
    let (input, script_len) = parse_varint(input)?;
    let (input, script_pubkey) = take(script_len)(input)?;
    Ok((
        input,
        TxOut {
            value,
            script_pubkey: script_pubkey.to_vec(),
        },
    ))
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], BitcoinTransaction> {
    let (input, version) = le_u32(input)?;
    let (input, input_count) = parse_varint(input)?;
    let (mut input, mut inputs) = (input, Vec::new());
    for _ in 0..input_count {
        let (new_input, tx_in) = parse_input(input)?;
        inputs.push(tx_in);
        input = new_input;
    }
    let (input, output_count) = parse_varint(input)?;
    let (mut input, mut outputs) = (input, Vec::new());
    for _ in 0..output_count {
        let (new_input, tx_out) = parse_output(input)?;
        outputs.push(tx_out);
        input = new_input;
    }
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

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <file_path>", args[0]);
        return;
    }
    let path = Path::new(&args[1]);
    let mut file = File::open(path).expect("File not found");
    let mut data = Vec::new();
    file.read_to_end(&mut data).expect("Failed to read file");

    match parse_transaction(&data) {
        Ok((_, transaction)) => println!("{:#?}", transaction),
        Err(e) => println!("Failed to parse transaction: {:?}", e),
    }
}