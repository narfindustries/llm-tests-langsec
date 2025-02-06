use nom::{
    bytes::complete::take,
    combinator::{map, opt},
    multi::length_count,
    number::complete::{be_u16, be_u32, be_u64},
    sequence::tuple,
    IResult,
};
use std::fs::read;
use std::path::Path;

#[derive(Debug)]
struct TxIn {
    prev_out: (Vec<u8>, u32),
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
    txins: Vec<TxIn>,
    txouts: Vec<TxOut>,
    locktime: u32,
}

fn read_varint(input: &[u8]) -> IResult<&[u8], u64> {
    let (rest, value) = be_u32(input)?;
    if value < 0xfd {
        Ok((rest, value as u64))
    } else if value == 0xfd {
        let (rest, value) = tuple((take(2u8), be_u16))(rest)?;
        Ok((rest, value as u64))
    } else if value == 0xfe {
        let (rest, value) = tuple((take(4u8), be_u32))(rest)?;
        Ok((rest, value as u64))
    } else {
        let (rest, value) = tuple((take(8u8), be_u64))(rest)?;
        Ok((rest, value))
    }
}

fn parse_txin(input: &[u8]) -> IResult<&[u8], TxIn> {
    let (rest, (hash, index, script_sig_len, script_sig, sequence)) = tuple((
        take(32u8),
        be_u32,
        read_varint,
        take(script_sig_len as usize),
        be_u32,
    ))(input)?;
    Ok((
        rest,
        TxIn {
            prev_out: (hash.to_vec(), index),
            script_sig: script_sig.to_vec(),
            sequence,
        },
    ))
}

fn parse_txout(input: &[u8]) -> IResult<&[u8], TxOut> {
    let (rest, (value, script_pubkey_len, script_pubkey)) = tuple((
        be_u64,
        read_varint,
        take(script_pubkey_len as usize),
    ))(input)?;
    Ok((
        rest,
        TxOut {
            value,
            script_pubkey: script_pubkey.to_vec(),
        },
    ))
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], BitcoinTransaction> {
    let (rest, (version, num_inputs, num_outputs, txins, txouts, locktime)) = tuple((
        be_u32,
        read_varint,
        read_varint,
        length_count(num_inputs as usize, parse_txin),
        length_count(num_outputs as usize, parse_txout),
        be_u32,
    ))(input)?;
    Ok((
        rest,
        BitcoinTransaction {
            version,
            txins,
            txouts,
            locktime,
        },
    ))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let path = Path::new(&args[1]);
    let data = match read(path) {
        Ok(data) => data,
        Err(err) => {
            eprintln!("Failed to read file: {}", err);
            return;
        }
    };

    match parse_transaction(&data) {
        Ok((rest, tx)) => {
            println!("Parsed transaction:\n{:#?}", tx);
            if !rest.is_empty() {
                println!("Remaining bytes: {:?}", rest);
            }
        }
        Err(e) => {
            eprintln!("Failed to parse transaction: {:?}", e);
        }
    }
}
