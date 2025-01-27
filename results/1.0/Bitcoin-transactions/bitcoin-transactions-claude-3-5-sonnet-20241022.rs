use nom::{
    bytes::complete::{tag, take},
    combinator::map,
    multi::count,
    number::complete::{le_u16, le_u32, le_u64, le_u8},
    sequence::tuple,
    IResult,
};
use std::{env, fs::File, io::Read};

#[derive(Debug)]
struct Transaction {
    version: u32,
    input_count: u64,
    inputs: Vec<TxInput>,
    output_count: u64,
    outputs: Vec<TxOutput>,
    lock_time: u32,
}

#[derive(Debug)]
struct TxInput {
    previous_output: OutPoint,
    script_length: u64,
    signature_script: Vec<u8>,
    sequence: u32,
}

#[derive(Debug)]
struct OutPoint {
    hash: [u8; 32],
    index: u32,
}

#[derive(Debug)]
struct TxOutput {
    value: u64,
    pk_script_length: u64,
    pk_script: Vec<u8>,
}

fn parse_varint(input: &[u8]) -> IResult<&[u8], u64> {
    let (input, first_byte) = le_u8(input)?;
    match first_byte {
        0..=0xfc => Ok((input, first_byte as u64)),
        0xfd => map(le_u16, |x| x as u64)(input),
        0xfe => map(le_u32, |x| x as u64)(input),
        0xff => le_u64(input),
    }
}

fn parse_outpoint(input: &[u8]) -> IResult<&[u8], OutPoint> {
    let (input, (hash, index)) = tuple((take(32usize), le_u32))(input)?;
    Ok((
        input,
        OutPoint {
            hash: hash.try_into().unwrap(),
            index,
        },
    ))
}

fn parse_tx_input(input: &[u8]) -> IResult<&[u8], TxInput> {
    let (input, previous_output) = parse_outpoint(input)?;
    let (input, script_length) = parse_varint(input)?;
    let (input, signature_script) = take(script_length as usize)(input)?;
    let (input, sequence) = le_u32(input)?;
    
    Ok((
        input,
        TxInput {
            previous_output,
            script_length,
            signature_script: signature_script.to_vec(),
            sequence,
        },
    ))
}

fn parse_tx_output(input: &[u8]) -> IResult<&[u8], TxOutput> {
    let (input, value) = le_u64(input)?;
    let (input, pk_script_length) = parse_varint(input)?;
    let (input, pk_script) = take(pk_script_length as usize)(input)?;
    
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
    let (input, input_count) = parse_varint(input)?;
    let (input, inputs) = count(parse_tx_input, input_count as usize)(input)?;
    let (input, output_count) = parse_varint(input)?;
    let (input, outputs) = count(parse_tx_output, output_count as usize)(input)?;
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
            if !remaining.is_empty() {
                println!("Warning: {} unparsed bytes remaining", remaining.len());
            }
        }
        Err(e) => eprintln!("Failed to parse transaction: {:?}", e),
    }
}