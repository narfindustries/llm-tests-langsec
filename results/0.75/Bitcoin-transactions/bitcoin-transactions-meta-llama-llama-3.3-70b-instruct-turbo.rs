use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::{length_data, many_till},
    number::complete::{be_u32, be_u64},
    sequence::{preceded, tuple},
    IResult,
};
use std::{
    fs::File,
    io::{BufReader, Read},
    path::Path,
};
use std::env;

#[derive(Debug)]
struct VarInt {
    value: u64,
}

impl VarInt {
    fn parse(input: &[u8]) -> IResult<&[u8], VarInt> {
        map_res(take(1usize), |value: &[u8]| {
            let first_byte = value[0];
            if first_byte < 0xFD {
                Ok(VarInt { value: first_byte as u64 })
            } else if first_byte == 0xFD {
                map_res(take(3usize), |value: &[u8]| {
                    let value = ((value[0] as u64) << 8) | (value[1] as u64) | ((value[2] as u64) << 16);
                    Ok(VarInt { value })
                })(input)
            } else if first_byte == 0xFE {
                map_res(take(5usize), |value: &[u8]| {
                    let value =
                        ((value[0] as u64) << 24) | ((value[1] as u64) << 16) | ((value[2] as u64) << 8)
                        | (value[3] as u64) | ((value[4] as u64) << 32);
                    Ok(VarInt { value })
                })(input)
            } else {
                map_res(take(9usize), |value: &[u8]| {
                    let value = ((value[0] as u64) << 56)
                        | ((value[1] as u64) << 48)
                        | ((value[2] as u64) << 40)
                        | ((value[3] as u64) << 32)
                        | ((value[4] as u64) << 24)
                        | ((value[5] as u64) << 16)
                        | ((value[6] as u64) << 8)
                        | (value[7] as u64);
                    Ok(VarInt { value })
                })(input)
            }
        })(input)
    }
}

#[derive(Debug)]
struct TransactionInput {
    previous_output: OutPoint,
    script_length: VarInt,
    script_signature: Vec<u8>,
    sequence: u32,
}

#[derive(Debug)]
struct OutPoint {
    hash: [u8; 32],
    index: u32,
}

impl OutPoint {
    fn parse(input: &[u8]) -> IResult<&[u8], OutPoint> {
        map(
            tuple((take(32usize), be_u32)),
            |(hash, index)| OutPoint { hash, index },
        )(input)
    }
}

impl TransactionInput {
    fn parse(input: &[u8]) -> IResult<&[u8], TransactionInput> {
        map(
            tuple((
                OutPoint::parse,
                VarInt::parse,
                length_data(VarInt::parse),
                be_u32,
            )),
            |(previous_output, script_length, script_signature, sequence)| {
                TransactionInput {
                    previous_output,
                    script_length,
                    script_signature,
                    sequence,
                }
            },
        )(input)
    }
}

#[derive(Debug)]
struct TransactionOutput {
    value: u64,
    pk_script_length: VarInt,
    pk_script: Vec<u8>,
}

impl TransactionOutput {
    fn parse(input: &[u8]) -> IResult<&[u8], TransactionOutput> {
        map(
            tuple((be_u64, VarInt::parse, length_data(VarInt::parse))),
            |(value, pk_script_length, pk_script)| TransactionOutput {
                value,
                pk_script_length,
                pk_script,
            },
        )(input)
    }
}

#[derive(Debug)]
struct Transaction {
    version: i32,
    num_inputs: VarInt,
    inputs: Vec<TransactionInput>,
    num_outputs: VarInt,
    outputs: Vec<TransactionOutput>,
    locktime: u32,
    witness: Vec<u8>,
}

impl Transaction {
    fn parse(input: &[u8]) -> IResult<&[u8], Transaction> {
        map(
            tuple((
                be_u32,
                VarInt::parse,
                many_till(TransactionInput::parse, VarInt::parse),
                VarInt::parse,
                many_till(TransactionOutput::parse, be_u32),
                be_u32,
                length_data(be_u32),
            )),
            |(version, num_inputs, inputs, num_outputs, outputs, locktime, witness)| {
                Transaction {
                    version,
                    num_inputs,
                    inputs,
                    num_outputs,
                    outputs,
                    locktime,
                    witness,
                }
            },
        )(input)
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = &args[1];
    let path = Path::new(file_path);
    let display = path.display();
    let file = match File::open(&path) {
        Err(why) => panic!("couldn't open {}: {}", display, why),
        Ok(file) => file,
    };
    let mut reader = BufReader::new(file);
    let mut data = Vec::new();
    reader.read_to_end(&mut data).unwrap();
    let transaction = Transaction::parse(&data).unwrap().1;
    println!("{:?}", transaction);
}