use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_opt},
    multi::{length_data, many0},
    number::complete::{le_u32, le_u64, le_u8, le_u16},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

// Bitcoin VarInt parser
fn varint(input: &[u8]) -> IResult<&[u8], u64> {
    let (input, prefix) = le_u8(input)?;
    match prefix {
        0xFD => map(le_u16, u64::from)(input),
        0xFE => le_u32(input).map(|(i, v)| (i, u64::from(v))),
        0xFF => le_u64(input),
        _ => Ok((input, u64::from(prefix))),
    }
}

// Bitcoin outpoint parser
fn outpoint(input: &[u8]) -> IResult<&[u8], OutPoint> {
    let (input, hash) = take(32usize)(input)?;
    let (input, index) = le_u32(input)?;
    Ok((input, OutPoint { hash: hash.to_vec(), index }))
}

// Bitcoin TxIn parser
fn txin(input: &[u8]) -> IResult<&[u8], TxIn> {
    let (input, previous_output) = outpoint(input)?;
    let (input, script_sig) = length_data(varint)(input)?;
    let (input, sequence) = le_u32(input)?;
    Ok((input, TxIn { previous_output, script_sig: script_sig.to_vec(), sequence }))
}

// Bitcoin TxOut parser
fn txout(input: &[u8]) -> IResult<&[u8], TxOut> {
    let (input, value) = le_u64(input)?;
    let (input, script_pubkey) = length_data(varint)(input)?;
    Ok((input, TxOut { value, script_pubkey: script_pubkey.to_vec() }))
}

// Bitcoin transaction parser
fn transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    let (input, version) = le_u32(input)?;
    let (input, flag) = map_opt(le_u8, |f| if f == 0x00 { Some(f) } else { None })(input)?;
    let (input, tx_in) = many0(txin)(input)?;
    let (input, tx_out) = many0(txout)(input)?;
    let (input, lock_time) = le_u32(input)?;
    Ok((input, Transaction { version, flag, tx_in, tx_out, lock_time }))
}

#[derive(Debug)]
struct OutPoint {
    hash: Vec<u8>,
    index: u32,
}

#[derive(Debug)]
struct TxIn {
    previous_output: OutPoint,
    script_sig: Vec<u8>,
    sequence: u32,
}

#[derive(Debug)]
struct TxOut {
    value: u64,
    script_pubkey: Vec<u8>,
}

#[derive(Debug)]
struct Transaction {
    version: u32,
    flag: u8,
    tx_in: Vec<TxIn>,
    tx_out: Vec<TxOut>,
    lock_time: u32,
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match transaction(&buffer) {
        Ok((_, tx)) => println!("{:?}", tx),
        Err(e) => eprintln!("Failed to parse transaction: {:?}", e),
    }

    Ok(())
}