use nom::{
    bytes::complete::{tag, take},
    combinator::map,
    multi::length_count,
    number::complete::{le_u32, le_u64},
    sequence::tuple,
    IResult,
};
use std::{
    env,
    fs::File,
    io::{self, Read},
};

#[derive(Debug)]
struct Transaction {
    version: u32,
    inputs: Vec<TxIn>,
    outputs: Vec<TxOut>,
    lock_time: u32,
}

#[derive(Debug)]
struct TxIn {
    previous_output: OutPoint,
    script: Vec<u8>,
    sequence: u32,
}

#[derive(Debug)]
struct OutPoint {
    hash: [u8; 32],
    index: u32,
}

#[derive(Debug)]
struct TxOut {
    value: u64,
    script: Vec<u8>,
}

fn parse_outpoint(input: &[u8]) -> IResult<&[u8], OutPoint> {
    let (input, hash) = take(32usize)(input)?;
    let (input, index) = le_u32(input)?;
    let mut outpoint_hash = [0u8; 32];
    outpoint_hash.copy_from_slice(hash);
    Ok((input, OutPoint { hash: outpoint_hash, index }))
}

fn parse_txin(input: &[u8]) -> IResult<&[u8], TxIn> {
    let (input, previous_output) = parse_outpoint(input)?;
    let (input, script_len) = nom::number::complete::varint(input)?;
    let (input, script) = take(script_len)(input)?;
    let (input, sequence) = le_u32(input)?;
    Ok((
        input,
        TxIn {
            previous_output,
            script: script.to_vec(),
            sequence,
        },
    ))
}

fn parse_txout(input: &[u8]) -> IResult<&[u8], TxOut> {
    let (input, value) = le_u64(input)?;
    let (input, script_len) = nom::number::complete::varint(input)?;
    let (input, script) = take(script_len)(input)?;
    Ok((
        input,
        TxOut {
            value,
            script: script.to_vec(),
        },
    ))
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    let (input, version) = le_u32(input)?;
    let (input, input_count) = nom::number::complete::varint(input)?;
    let (input, inputs) = length_count(map(input_count, |v| v as usize), parse_txin)(input)?;
    let (input, output_count) = nom::number::complete::varint(input)?;
    let (input, outputs) = length_count(map(output_count, |v| v as usize), parse_txout)(input)?;
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

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <binary file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_transaction(&buffer) {
        Ok((_, transaction)) => println!("{:#?}", transaction),
        Err(e) => eprintln!("Failed to parse transaction: {:?}", e),
    }

    Ok(())
}

mod varint {
    use nom::{
        combinator::map,
        error::{ErrorKind, ParseError},
        multi::length_data,
        number::complete::{be_u16, be_u32, be_u64, be_u8},
        IResult,
    };

    pub fn varint(input: &[u8]) -> IResult<&[u8], u64> {
        let (input, first_byte) = be_u8(input)?;
        match first_byte {
            0xFF => map(be_u64, |v| v)(input),
            0xFE => map(be_u32, |v| v as u64)(input),
            0xFD => map(be_u16, |v| v as u64)(input),
            _ => Ok((input, first_byte as u64)),
        }
    }
}