use std::env;
use std::fs::File;
use std::io::{Read, BufReader};
use nom::{
    IResult,
    bytes::complete::{take, tag},
    combinator::{map, map_res},
    number::complete::{be_u8, be_u16, be_u32, be_u64},
    multi::{many0, length_data},
    sequence::{tuple, preceded},
};

#[derive(Debug)]
enum TxIn {
    Coinbase {
        script_sig: Vec<u8>,
    },
    Regular {
        prev_out: Outpoint,
        script_sig: Vec<u8>,
        sequence: u32,
    },
}

#[derive(Debug)]
struct Outpoint {
    txid: [u8; 32],
    vout: u32,
}

#[derive(Debug)]
enum TxOut {
    Standard {
        value: u64,
        script_pub_key: Vec<u8>,
    },
}

#[derive(Debug)]
struct Tx {
    version: u32,
    tx_in_count: u8,
    tx_in: Vec<TxIn>,
    tx_out_count: u8,
    tx_out: Vec<TxOut>,
    locktime: u32,
}

fn parse_varint(input: &[u8]) -> IResult<&[u8], u64> {
    map_res(be_u8, |x| {
        if x < 0xfd {
            Ok(x as u64)
        } else if x == 0xfd {
            map_res(take(2_usize), |x| std::convert::TryInto::try_into(x)).map(|x: [u8; 2]| u64::from_be_bytes(x))
        } else if x == 0xfe {
            map_res(take(4_usize), |x| std::convert::TryInto::try_into(x)).map(|x: [u8; 4]| u64::from_be_bytes(x))
        } else {
            map_res(take(8_usize), |x| std::convert::Try_into::try_into(x)).map(|x: [u8; 8]| u64::from_be_bytes(x))
        }
    })(input)
}

fn parse_tx_in(input: &[u8]) -> IResult<&[u8], TxIn> {
    map(
        tuple((
            take(32_usize),
            be_u32,
            length_data(parse_varint),
            be_u32,
        )),
        |(txid, vout, script_sig, sequence)| {
            TxIn::Regular {
                prev_out: Outpoint { txid: txid.try_into().unwrap(), vout },
                script_sig: script_sig.to_vec(),
                sequence,
            }
        },
    )(input)
}

fn parse_tx_out(input: &[u8]) -> IResult<&[u8], TxOut> {
    map(
        tuple((
            be_u64,
            length_data(parse_varint),
        )),
        |(value, script_pub_key)| {
            TxOut::Standard {
                value,
                script_pub_key: script_pub_key.to_vec(),
            }
        },
    )(input)
}

fn parse_tx(input: &[u8]) -> IResult<&[u8], Tx> {
    map(
        tuple((
            be_u32,
            parse_varint,
            many0(parse_tx_in),
            parse_varint,
            many0(parse_tx_out),
            be_u32,
        )),
        |(version, tx_in_count, tx_in, tx_out_count, tx_out, locktime)| {
            Tx {
                version,
                tx_in_count: tx_in_count as u8,
                tx_in,
                tx_out_count: tx_out_count as u8,
                tx_out,
                locktime,
            }
        },
    )(input)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <input_file>", args[0]);
    }
    let input_file = &args[1];
    let file = File::open(input_file).unwrap();
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input).unwrap();
    println!("{:?}", parse_tx(&input).unwrap().1);
}