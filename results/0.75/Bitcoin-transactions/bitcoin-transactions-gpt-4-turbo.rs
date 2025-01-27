use nom::{
    bytes::complete::take,
    number::complete::{le_u32, le_u64, le_u8},
    IResult,
};
use std::{env, fs::File, io::Read, path::Path};

#[derive(Debug)]
struct TxIn {
    previous_output: OutPoint,
    script_sig: Vec<u8>,
    sequence: u32,
}

#[derive(Debug)]
struct TxOut {
    value: u64,
    script_pub_key: Vec<u8>,
}

#[derive(Debug)]
struct OutPoint {
    hash: Vec<u8>,
    index: u32,
}

#[derive(Debug)]
struct Transaction {
    version: u32,
    inputs: Vec<TxIn>,
    outputs: Vec<TxOut>,
    lock_time: u32,
}

fn parse_outpoint(input: &[u8]) -> IResult<&[u8], OutPoint> {
    let (input, hash) = take(32usize)(input)?;
    let (input, index) = le_u32(input)?;
    Ok((input, OutPoint { hash: hash.to_vec(), index }))
}

fn parse_txin(input: &[u8]) -> IResult<&[u8], TxIn> {
    let (input, previous_output) = parse_outpoint(input)?;
    let (input, script_len) = le_u64(input)?;
    let (input, script_sig) = take(script_len)(input)?;
    let (input, sequence) = le_u32(input)?;
    Ok((
        input,
        TxIn {
            previous_output,
            script_sig: script_sig.to_vec(),
            sequence,
        },
    ))
}

fn parse_txout(input: &[u8]) -> IResult<&[u8], TxOut> {
    let (input, value) = le_u64(input)?;
    let (input, pk_script_len) = le_u64(input)?;
    let (input, script_pub_key) = take(pk_script_len)(input)?;
    Ok((
        input,
        TxOut {
            value,
            script_pub_key: script_pub_key.to_vec(),
        },
    ))
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    let (input, version) = le_u32(input)?;
    let (input, in_count) = le_u64(input)?;
    let (mut input, mut inputs) = (input, Vec::new());
    for _ in 0..in_count {
        let (inner_input, tx_in) = parse_txin(input)?;
        input = inner_input;
        inputs.push(tx_in);
    }
    let (input, out_count) = le_u64(input)?;
    let (mut input, mut outputs) = (input, Vec::new());
    for _ in 0..out_count {
        let (inner_input, tx_out) = parse_txout(input)?;
        input = inner_input;
        outputs.push(tx_out);
    }
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

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file_path>", args[0]);
        std::process::exit(1);
    }
    let path = Path::new(&args[1]);
    let mut file = File::open(&path)?;
    let mut data = Vec::new();
    file.read_to_end(&mut data)?;

    match parse_transaction(&data) {
        Ok((_, transaction)) => {
            println!("Parsed Transaction: {:?}", transaction);
        }
        Err(e) => {
            eprintln!("Failed to parse transaction: {:?}", e);
        }
    }

    Ok(())
}