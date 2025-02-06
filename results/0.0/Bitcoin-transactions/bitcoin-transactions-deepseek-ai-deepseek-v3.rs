use nom::{
    bytes::complete::take,
    combinator::map,
    multi::length_count,
    number::complete::{le_u32, le_u64},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct Transaction {
    version: u32,
    inputs: Vec<Input>,
    outputs: Vec<Output>,
    lock_time: u32,
    witness: Option<Vec<Vec<u8>>>,
}

#[derive(Debug)]
struct Input {
    previous_tx_hash: Vec<u8>,
    previous_output_index: u32,
    script_sig: Vec<u8>,
    sequence: u32,
}

#[derive(Debug)]
struct Output {
    value: u64,
    script_pub_key: Vec<u8>,
}

fn parse_varint(input: &[u8]) -> IResult<&[u8], u64> {
    let (input, first_byte) = take(1u8)(input)?;
    let first_byte = first_byte[0];
    match first_byte {
        0xFD => map(take(2u8), |bytes: &[u8]| u16::from_le_bytes([bytes[0], bytes[1]]) as u64)(input),
        0xFE => map(take(4u8), |bytes: &[u8]| u32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]) as u64)(input),
        0xFF => map(take(8u8), |bytes: &[u8]| u64::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7]]))(input),
        _ => Ok((input, first_byte as u64)),
    }
}

fn parse_input(input: &[u8]) -> IResult<&[u8], Input> {
    let (input, previous_tx_hash) = take(32u8)(input)?;
    let (input, previous_output_index) = le_u32(input)?;
    let (input, script_sig_len) = parse_varint(input)?;
    let (input, script_sig) = take(script_sig_len)(input)?;
    let (input, sequence) = le_u32(input)?;
    Ok((
        input,
        Input {
            previous_tx_hash: previous_tx_hash.to_vec(),
            previous_output_index,
            script_sig: script_sig.to_vec(),
            sequence,
        },
    ))
}

fn parse_output(input: &[u8]) -> IResult<&[u8], Output> {
    let (input, value) = le_u64(input)?;
    let (input, script_pub_key_len) = parse_varint(input)?;
    let (input, script_pub_key) = take(script_pub_key_len)(input)?;
    Ok((
        input,
        Output {
            value,
            script_pub_key: script_pub_key.to_vec(),
        },
    ))
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    let (input, version) = le_u32(input)?;
    let (input, has_witness) = map(take(1u8), |bytes: &[u8]| bytes[0] == 0x00)(input)?;
    let (input, input_count) = parse_varint(input)?;
    let (input, inputs) = length_count(map(parse_varint, |x| x as usize), parse_input)(input)?;
    let (input, output_count) = parse_varint(input)?;
    let (input, outputs) = length_count(map(parse_varint, |x| x as usize), parse_output)(input)?;
    let (input, witness) = if has_witness {
        let (input, witness_data) = length_count(map(parse_varint, |x| x as usize), |input| {
            let (input, len) = parse_varint(input)?;
            take(len)(input)
        })(input)?;
        (input, Some(witness_data.into_iter().map(|x| x.to_vec()).collect()))
    } else {
        (input, None)
    };
    let (input, lock_time) = le_u32(input)?;
    Ok((
        input,
        Transaction {
            version,
            inputs,
            outputs,
            lock_time,
            witness,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_transaction(&buffer) {
        Ok((_, transaction)) => println!("{:#?}", transaction),
        Err(e) => eprintln!("Failed to parse transaction: {:?}", e),
    }
}