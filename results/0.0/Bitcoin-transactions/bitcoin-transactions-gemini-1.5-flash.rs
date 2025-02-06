use nom::{
    bytes::complete::take,
    combinator::{map, opt},
    multi::count,
    number::complete::{be_i32, be_i64, be_u32, be_u8},
    IResult,
};
use std::fs;
use std::path::Path;

#[derive(Debug)]
struct Transaction {
    version: i32,
    inputs: Vec<TxIn>,
    outputs: Vec<TxOut>,
    locktime: u32,
    witness: Option<Vec<Vec<Vec<u8>>>>,
}

#[derive(Debug)]
struct TxIn {
    prev_out: (Vec<u8>, u32),
    script_sig: Vec<u8>,
    sequence: u32,
}

#[derive(Debug)]
struct TxOut {
    value: i64,
    script_pubkey: Vec<u8>,
}

fn parse_varint(input: &[u8]) -> IResult<&[u8], u64> {
    let (rest, first) = be_u8(input)?;
    match first {
        0xfd => map(take(2usize), |bytes: &[u8]| u64::from_be_bytes(bytes.try_into().unwrap()))(rest),
        0xfe => map(take(4usize), |bytes: &[u8]| u64::from_be_bytes(bytes.try_into().unwrap()))(rest),
        0xff => map(take(8usize), |bytes: &[u8]| u64::from_be_bytes(bytes.try_into().unwrap()))(rest),
        _ => Ok((rest, first as u64)),
    }
}

fn parse_bytes(len: u64, input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    map(take(len as usize), |bytes: &[u8]| bytes.to_vec())(input)
}

fn parse_txin(input: &[u8]) -> IResult<&[u8], TxIn> {
    let (rest, prev_out_hash) = take(32usize)(input)?;
    let (rest, prev_out_index) = be_u32(rest)?;
    let (rest, script_sig_len) = parse_varint(rest)?;
    let (rest, script_sig) = parse_bytes(script_sig_len, rest)?;
    let (rest, sequence) = be_u32(rest)?;
    Ok((
        rest,
        TxIn {
            prev_out: (prev_out_hash.to_vec(), prev_out_index),
            script_sig,
            sequence,
        },
    ))
}

fn parse_txout(input: &[u8]) -> IResult<&[u8], TxOut> {
    let (rest, value) = be_i64(input)?;
    let (rest, script_pubkey_len) = parse_varint(rest)?;
    let (rest, script_pubkey) = parse_bytes(script_pubkey_len, rest)?;
    Ok((rest, TxOut { value, script_pubkey }))
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    let (rest, version) = be_i32(input)?;
    let (rest, num_inputs) = parse_varint(rest)?;
    let (rest, inputs) = count(parse_txin, num_inputs as usize)(rest)?;
    let (rest, num_outputs) = parse_varint(rest)?;
    let (rest, outputs) = count(parse_txout, num_outputs as usize)(rest)?;
    let (rest, locktime) = be_u32(rest)?;
    let (rest, witness) = opt(|i| {
        let (rest, num_witnesses) = parse_varint(i)?;
        count(
            |i| {
                let (rest, num_items) = parse_varint(i)?;
                count(
                    |i| {
                        let (rest, item_len) = parse_varint(i)?;
                        parse_bytes(item_len, rest)
                    },
                    num_items as usize,
                )(rest)
            },
            num_witnesses as usize,
        )(rest)
    })(rest)?;

    Ok((
        rest,
        Transaction {
            version,
            inputs,
            outputs,
            locktime,
            witness,
        },
    ))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let contents = fs::read(path).expect("Failed to read file");

    match parse_transaction(&contents) {
        Ok((_, transaction)) => println!("{:#?}", transaction),
        Err(e) => eprintln!("Error parsing transaction: {:?}", e),
    }
}
