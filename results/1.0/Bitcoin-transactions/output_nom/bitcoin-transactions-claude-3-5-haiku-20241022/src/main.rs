use std::env;
use std::fs::File;
use std::io::Read;
use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_opt},
    multi::{count, many0, length_count},
    number::complete::{le_u8, le_u16, le_u32, le_u64},
    sequence::{tuple, preceded},
    IResult,
};

#[derive(Debug)]
struct OutPoint {
    txid: [u8; 32],
    index: u32,
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
    amount: u64,
    pk_script_length: u64,
    pk_script: Vec<u8>,
}

#[derive(Debug)]
struct Transaction {
    version: u32,
    tx_in_count: u64,
    tx_ins: Vec<TxIn>,
    tx_out_count: u64,
    tx_outs: Vec<TxOut>,
    locktime: u32,
    witness: Option<Vec<WitnessData>>,
}

#[derive(Debug)]
struct WitnessData {
    witness_item_count: u64,
    witness_items: Vec<Vec<u8>>,
}

fn parse_outpoint(input: &[u8]) -> IResult<&[u8], OutPoint> {
    map(
        tuple((take(32usize), le_u32)),
        |(txid, index)| OutPoint {
            txid: txid.try_into().unwrap(),
            index,
        }
    )(input)
}

fn parse_tx_in(input: &[u8]) -> IResult<&[u8], TxIn> {
    map(
        tuple((
            parse_outpoint,
            nom::number::complete::var_int,
            map_opt(take_var_len_bytes, |v| Some(v)),
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
            map_opt(take_var_len_bytes, |v| Some(v))
        )),
        |(amount, pk_script_length, pk_script)| TxOut {
            amount,
            pk_script_length,
            pk_script,
        }
    )(input)
}

fn parse_witness(input: &[u8]) -> IResult<&[u8], WitnessData> {
    map(
        tuple((
            nom::number::complete::var_int,
            length_count(nom::combinator::success(input.len()), 
                map_opt(take_var_len_bytes, |v| Some(v)))
        )),
        |(witness_item_count, witness_items)| WitnessData {
            witness_item_count,
            witness_items,
        }
    )(input)
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    map(
        tuple((
            le_u32,
            preceded(
                tag([0x00]),  // marker
                nom::number::complete::var_int
            ),
            count(parse_tx_in, 1),
            nom::number::complete::var_int,
            count(parse_tx_out, 1),
            le_u32,
            nom::combinator::opt(parse_witness)
        )),
        |(version, tx_in_count, tx_ins, tx_out_count, tx_outs, locktime, witness)| Transaction {
            version,
            tx_in_count,
            tx_ins,
            tx_out_count,
            tx_outs,
            locktime,
            witness,
        }
    )(input)
}

fn take_var_len_bytes(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, length) = nom::number::complete::var_int(input)?;
    take(length)(input)
}

fn main() -> std::io::Result<()> {
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
            println!("Parsed Transaction: {:#?}", transaction);
            Ok(())
        },
        Err(e) => {
            eprintln!("Failed to parse transaction: {:?}", e);
            std::process::exit(1);
        }
    }
}