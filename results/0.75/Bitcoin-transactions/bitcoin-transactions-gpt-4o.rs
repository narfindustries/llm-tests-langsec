use nom::{
    bytes::complete::take,
    combinator::map_opt,
    multi::count,
    number::complete::{le_u32, le_u64, le_u8},
    IResult,
};
use std::env;
use std::fs;
use std::io::{self, Read};

#[derive(Debug)]
struct Transaction {
    version: u32,
    inputs: Vec<Input>,
    outputs: Vec<Output>,
    locktime: u32,
}

#[derive(Debug)]
struct Input {
    prev_tx_hash: [u8; 32],
    output_index: u32,
    script_sig: Vec<u8>,
    sequence: u32,
}

#[derive(Debug)]
struct Output {
    value: u64,
    script_pubkey: Vec<u8>,
}

fn parse_varint(input: &[u8]) -> IResult<&[u8], u64> {
    let (input, first_byte) = le_u8(input)?;
    match first_byte {
        0xFD => {
            let (input, value) = nom::number::complete::le_u16(input)?;
            Ok((input, value as u64))
        }
        0xFE => {
            let (input, value) = nom::number::complete::le_u32(input)?;
            Ok((input, value as u64))
        }
        0xFF => nom::number::complete::le_u64(input),
        _ => Ok((input, first_byte as u64)),
    }
}

fn parse_input(input: &[u8]) -> IResult<&[u8], Input> {
    let (input, prev_tx_hash) = take(32usize)(input)?;
    let (input, output_index) = le_u32(input)?;
    let (input, script_len) = parse_varint(input)?;
    let (input, script_sig) = take(script_len)(input)?;
    let (input, sequence) = le_u32(input)?;
    Ok((
        input,
        Input {
            prev_tx_hash: {
                let mut hash = [0u8; 32];
                hash.copy_from_slice(prev_tx_hash);
                hash
            },
            output_index,
            script_sig: script_sig.to_vec(),
            sequence,
        },
    ))
}

fn parse_output(input: &[u8]) -> IResult<&[u8], Output> {
    let (input, value) = le_u64(input)?;
    let (input, script_len) = parse_varint(input)?;
    let (input, script_pubkey) = take(script_len)(input)?;
    Ok((
        input,
        Output {
            value,
            script_pubkey: script_pubkey.to_vec(),
        },
    ))
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    let (input, version) = le_u32(input)?;

    let (input, marker_or_input_count) = le_u8(input)?;
    let (input, flag_or_remaining) = le_u8(input)?;

    let (input, inputs, is_segwit) = if marker_or_input_count == 0 && flag_or_remaining == 1 {
        let (input, input_count) = parse_varint(input)?;
        let (input, inputs) = count(parse_input, input_count as usize)(input)?;
        (input, inputs, true)
    } else {
        let (remaining, inputs) = count(parse_input, marker_or_input_count as usize)(input)?;
        (remaining, inputs, false)
    };

    let (input, output_count) = parse_varint(input)?;
    let (input, outputs) = count(parse_output, output_count as usize)(input)?;

    let (input, locktime) = le_u32(input)?;

    // SegWit transactions have witness data here, but we just ignore it for simplicity.

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

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return Ok(());
    }

    let filename = &args[1];
    let mut file = fs::File::open(filename)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_transaction(&buffer) {
        Ok((_, tx)) => println!("{:?}", tx),
        Err(e) => eprintln!("Failed to parse transaction: {:?}", e),
    }

    Ok(())
}