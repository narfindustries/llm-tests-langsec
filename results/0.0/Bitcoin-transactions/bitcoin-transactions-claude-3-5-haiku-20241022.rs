use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{count, many0},
    number::complete::{le_u16, le_u32, le_u64},
    sequence::{tuple, preceded},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct OutPoint {
    txid: [u8; 32],
    vout: u32,
}

#[derive(Debug)]
struct TxIn {
    previous_output: OutPoint,
    script_sig_length: u64,
    script_sig: Vec<u8>,
    sequence: u32,
}

#[derive(Debug)]
struct TxOut {
    value: u64,
    script_pubkey_length: u64,
    script_pubkey: Vec<u8>,
}

#[derive(Debug)]
struct Transaction {
    version: u32,
    marker: Option<u8>,
    flag: Option<u8>,
    tx_in_count: u64,
    tx_ins: Vec<TxIn>,
    tx_out_count: u64,
    tx_outs: Vec<TxOut>,
    witness_data: Option<Vec<Vec<u8>>>,
    locktime: u32,
}

fn parse_outpoint(input: &[u8]) -> IResult<&[u8], OutPoint> {
    map(
        tuple((take(32usize), le_u32)),
        |(txid, vout)| OutPoint {
            txid: txid.try_into().unwrap(),
            vout,
        }
    )(input)
}

fn parse_tx_in(input: &[u8]) -> IResult<&[u8], TxIn> {
    map(
        tuple((
            parse_outpoint,
            nom::number::complete::var_int,
            take_var_int_bytes,
            le_u32
        )),
        |(previous_output, script_sig_length, script_sig, sequence)| TxIn {
            previous_output,
            script_sig_length,
            script_sig,
            sequence,
        }
    )(input)
}

fn parse_tx_out(input: &[u8]) -> IResult<&[u8], TxOut> {
    map(
        tuple((
            le_u64,
            nom::number::complete::var_int,
            take_var_int_bytes
        )),
        |(value, script_pubkey_length, script_pubkey)| TxOut {
            value,
            script_pubkey_length,
            script_pubkey,
        }
    )(input)
}

fn take_var_int_bytes(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, length) = nom::number::complete::var_int(input)?;
    take(length)(input)
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    let (input, version) = le_u32(input)?;
    let (input, (marker, flag)) = opt(tuple((tag(&[0x00]), tag(&[0x01]))))(input)?;

    let (input, tx_in_count) = nom::number::complete::var_int(input)?;
    let (input, tx_ins) = count(parse_tx_in, tx_in_count as usize)(input)?;

    let (input, tx_out_count) = nom::number::complete::var_int(input)?;
    let (input, tx_outs) = count(parse_tx_out, tx_out_count as usize)(input)?;

    let (input, witness_data) = if marker.is_some() && flag.is_some() {
        let (input, witness_items) = many0(|i| {
            let (input, witness_item_length) = nom::number::complete::var_int(i)?;
            take(witness_item_length)(input)
        })(input)?;
        (input, Some(witness_items))
    } else {
        (input, None)
    };

    let (input, locktime) = le_u32(input)?;

    Ok((input, Transaction {
        version,
        marker,
        flag,
        tx_in_count,
        tx_ins,
        tx_out_count,
        tx_outs,
        witness_data,
        locktime,
    }))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <transaction_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_transaction(&buffer) {
        Ok((_, transaction)) => {
            println!("Parsed Transaction: {:?}", transaction);
            Ok(())
        }
        Err(e) => {
            eprintln!("Failed to parse transaction: {:?}", e);
            std::process::exit(1);
        }
    }
}