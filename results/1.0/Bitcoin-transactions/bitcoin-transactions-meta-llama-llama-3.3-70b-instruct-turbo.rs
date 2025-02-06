use nom::branch::alt;
use nom::bytes::complete::{tag, take};
use nom::combinator::{map, map_res, verify};
use nom::error::{context, ErrorKind};
use nom::multi::{many1};
use nom::number::complete::{le_u16, le_u32, le_u64};
use nom::sequence::{preceded, tuple};
use nom::IResult;
use std::env;
use std::fs::File;
use std::io::Read;
use std::path::Path;

#[derive(Debug)]
enum VarInt {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
}

fn var_int(input: &[u8]) -> IResult<&[u8], VarInt> {
    alt((
        map(tag([0xfd]), |_| VarInt::U16(253)),
        map(tag([0xfe]), |_| VarInt::U32(254)),
        map(tag([0xff]), |_| VarInt::U64(255)),
        map(take(1u8), |v: &[u8]| VarInt::U8(v[0])),
    ))(input)
}

fn parse_var_int(input: &[u8]) -> IResult<&[u8], u32> {
    context(
        "var_int",
        alt((
            map_res(take(1u8), |v: &[u8]| std::convert::TryInto::try_into(v[0]).map_err(|_| nom::Err::Error(ErrorKind::AlphaNumeric))),
            preceded(tag([0xfd]), map(le_u16, |v| v as u32)),
            preceded(tag([0xfe]), le_u32),
            preceded(tag([0xff]), map(le_u64, |v| v as u32)),
        )),
    )(input)
}

#[derive(Debug)]
enum Transaction {
    V1 {
        version: u32,
        tx_in: Vec<TxIn>,
        tx_out: Vec<TxOut>,
        lock_time: u32,
    },
    V2 {
        version: u32,
        tx_in: Vec<TxIn>,
        tx_out: Vec<TxOut>,
        lock_time: u32,
    },
}

#[derive(Debug)]
struct TxIn {
    previous_output_hash: [u8; 32],
    previous_output_index: u32,
    script_length: u32,
    script_sig: Vec<u8>,
    sequence: u32,
}

impl TxIn {
    fn new(previous_output_hash: [u8; 32], previous_output_index: u32, script_length: u32, script_sig: Vec<u8>, sequence: u32) -> Self {
        TxIn {
            previous_output_hash,
            previous_output_index,
            script_length,
            script_sig,
            sequence,
        }
    }
}

#[derive(Debug)]
struct TxOut {
    value: u64,
    pk_script_length: u32,
    pk_script: Vec<u8>,
}

impl TxOut {
    fn new(value: u64, pk_script_length: u32, pk_script: Vec<u8>) -> Self {
        TxOut {
            value,
            pk_script_length,
            pk_script,
        }
    }
}

fn parse_tx_in(input: &[u8]) -> IResult<&[u8], TxIn> {
    context(
        "tx_in",
        tuple((
            take(32u8),
            le_u32,
            parse_var_int,
            verify(
                parse_var_int,
                |script_length: &u32| *script_length > 0,
            ),
            take(1),
            le_u32,
        )),
    )(input)
    .map(|(input, (previous_output_hash, previous_output_index, script_length, _, script_sig_length, sequence))| {
        (
            input,
            TxIn::new(
                {
                    let mut arr = [0u8; 32];
                    arr.copy_from_slice(previous_output_hash);
                    arr
                },
                previous_output_index,
                script_length,
                vec![0],
                sequence,
            )
        )
    })
}

fn parse_tx_out(input: &[u8]) -> IResult<&[u8], TxOut> {
    context(
        "tx_out",
        tuple((
            le_u64,
            parse_var_int,
            verify(
                parse_var_int,
                |pk_script_length: &u32| *pk_script_length > 0,
            ),
        )),
    )(input)
    .map(|(input, (value, _, pk_script_length))| {
        (
            input,
            TxOut::new(
                value,
                pk_script_length,
                vec![],
            )
        )
    })
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    alt((
        map(
            tuple((
                le_u32,
                parse_var_int,
                many1(parse_tx_in),
                parse_var_int,
                many1(parse_tx_out),
                le_u32,
            )),
            |(version, _tx_in_count, tx_in, _tx_out_count, tx_out, lock_time)| {
                Transaction::V1 {
                    version,
                    tx_in,
                    tx_out,
                    lock_time,
                }
            },
        ),
        map(
            tuple((
                le_u32,
                parse_var_int,
                many1(parse_tx_in),
                parse_var_int,
                many1(parse_tx_out),
                le_u32,
            )),
            |(version, _tx_in_count, tx_in, _tx_out_count, tx_out, lock_time)| {
                Transaction::V2 {
                    version,
                    tx_in,
                    tx_out,
                    lock_time,
                }
            },
        ),
    ))(input)
}

fn main() {
    let file_path = env::args().nth(1).expect("no file path provided");
    let path = Path::new(&file_path);
    let mut file = match File::open(&path) {
        Err(why) => panic!("couldn't open {}: {}", path.display(), why),
        Ok(file) => file,
    };

    let mut data = Vec::new();
    file.read_to_end(&mut data).expect("couldn't read file");
    let result = parse_transaction(&data);
    match result {
        Ok((remaining, transaction)) => {
            println!("Transaction: {:?}", transaction);
            println!("Remaining: {:?}", remaining);
        }
        Err(err) => {
            println!("Error: {:?}", err);
        }
    }
}