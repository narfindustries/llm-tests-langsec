use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::{length_data, many_till},
    number::complete::{be_u32, be_u64, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
};

#[derive(Debug)]
enum TransactionInput {
    Coinbase {
        script_sig: Vec<u8>,
    },
    Regular {
        prev_out: OutPoint,
        script_sig: Vec<u8>,
        sequence: u32,
    },
}

#[derive(Debug)]
struct OutPoint {
    txid: [u8; 32],
    vout: u32,
}

#[derive(Debug)]
enum TransactionOutput {
    Regular {
        value: u64,
        script_pub_key: Vec<u8>,
    },
}

#[derive(Debug)]
struct Transaction {
    version: u32,
    inputs: Vec<TransactionInput>,
    outputs: Vec<TransactionOutput>,
    lock_time: u32,
}

fn parse_transaction_input(input: &[u8]) -> IResult<&[u8], TransactionInput> {
    preceded(
        be_u32,
        tuple((
            parse_out_point,
            length_data(map_res(be_u8, |x| std::str::from_utf8(x.as_slice()).map_err(|_| nom::Err::Error(())))),
            be_u32,
        )),
    )(input)
    .map(|(input, (prev_out, script_sig, sequence))| {
        (
            input,
            TransactionInput::Regular {
                prev_out,
                script_sig,
                sequence,
            },
        )
    })
    .or_else(|_| {
        map(
            tuple((take(32u8), length_data(map_res(be_u8, |x| std::str::from_utf8(x.as_slice()).map_err(|_| nom::Err::Error(())))))),
            |(txid, script_sig)| {
                (
                    &input[36..],
                    TransactionInput::Coinbase {
                        script_sig,
                    },
                )
            },
        )(input)
    })
}

fn parse_out_point(input: &[u8]) -> IResult<&[u8], OutPoint> {
    map(
        tuple((take(32u8), be_u32)),
        |(txid, vout)| OutPoint {
            txid: txid.try_into().unwrap(),
            vout,
        },
    )(input)
}

fn parse_transaction_output(input: &[u8]) -> IResult<&[u8], TransactionOutput> {
    map(
        tuple((be_u64, length_data(map_res(be_u8, |x| std::str::from_utf8(x.as_slice()).map_err(|_| nom::Err::Error(())))))),
        |(value, script_pub_key)| TransactionOutput::Regular { value, script_pub_key },
    )(input)
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    tuple((
        be_u32,
        many_till(
            parse_transaction_input,
            tuple((many_till(parse_transaction_output, be_u32), be_u32)),
        ),
    ))(input)
    .map(|(input, (version, (inputs, (outputs, lock_time))))| {
        (
            input,
            Transaction {
                version,
                inputs,
                outputs,
                lock_time,
            },
        )
    })
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <input_file>", args[0]);
    }
    let file = File::open(&args[1]).unwrap();
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input).unwrap();
    let (_rest, transaction) = parse_transaction(&input).unwrap();
    println!("{:?}", transaction);
}