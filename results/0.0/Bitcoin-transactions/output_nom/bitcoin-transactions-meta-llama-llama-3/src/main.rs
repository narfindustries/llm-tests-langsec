use nom::{
    bytes::complete::{take, take_while_m_n},
    combinator::{map, map_res, opt},
    error::{Error, ErrorKind},
    multi::{many0, many1},
    number::complete::{be_u32, be_u64, be_u8},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
};

#[derive(Debug, PartialEq)]
enum OpCode {
    Op0,
    OpPushdata1,
    OpPushdata2,
    OpPushdata4,
    Op1negate,
    Op1,
    Op2,
    Op3,
    Op4,
    Op5,
    Op6,
    Op7,
    Op8,
    Op9,
    Op10,
    Op11,
    Op12,
    Op13,
    Op14,
    Op15,
    Op16,
}

impl OpCode {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, op_code) = be_u8(input)?;
        match op_code {
            0x00 => Ok((input, OpCode::Op0)),
            0x4c => Ok((input, OpCode::OpPushdata1)),
            0x4d => Ok((input, OpCode::OpPushdata2)),
            0x4e => Ok((input, OpCode::OpPushdata4)),
            0x4f => Ok((input, OpCode::Op1negate)),
            0x51 => Ok((input, OpCode::Op1)),
            0x52 => Ok((input, OpCode::Op2)),
            0x53 => Ok((input, OpCode::Op3)),
            0x54 => Ok((input, OpCode::Op4)),
            0x55 => Ok((input, OpCode::Op5)),
            0x56 => Ok((input, OpCode::Op6)),
            0x57 => Ok((input, OpCode::Op7)),
            0x58 => Ok((input, OpCode::Op8)),
            0x59 => Ok((input, OpCode::Op9)),
            0x5a => Ok((input, OpCode::Op10)),
            0x5b => Ok((input, OpCode::Op11)),
            0x5c => Ok((input, OpCode::Op12)),
            0x5d => Ok((input, OpCode::Op13)),
            0x5e => Ok((input, OpCode::Op14)),
            0x5f => Ok((input, OpCode::Op15)),
            0x60 => Ok((input, OpCode::Op16)),
            _ => Err(nom::Err::Error((input, ErrorKind::AlphaNumeric))),
        }
    }
}

#[derive(Debug, PartialEq)]
struct Script {
    op_code: OpCode,
    data: Vec<u8>,
}

impl Script {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, op_code) = OpCode::parse(input)?;
        let (input, data) = match op_code {
            OpCode::OpPushdata1 => {
                let (input, len) = be_u8(input)?;
                take(len as usize)(input)
            }
            OpCode::OpPushdata2 => {
                let (input, len) = be_u16(input)?;
                take(len as usize)(input)
            }
            OpCode::OpPushdata4 => {
                let (input, len) = be_u32(input)?;
                take(len as usize)(input)
            }
            _ => Ok((input, Vec::new())),
        }?;
        Ok((input, Script { op_code, data: data.to_vec() }))
    }
}

#[derive(Debug, PartialEq)]
struct Input {
    previous_transaction_hash: [u8; 32],
    previous_output_index: u32,
    script: Script,
    sequence_number: u32,
}

impl Input {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, previous_transaction_hash) = take(32u8)(input)?;
        let (input, previous_output_index) = be_u32(input)?;
        let (input, script) = Script::parse(input)?;
        let (input, sequence_number) = be_u32(input)?;
        Ok((
            input,
            Input {
                previous_transaction_hash: previous_transaction_hash.try_into().unwrap(),
                previous_output_index,
                script,
                sequence_number,
            },
        ))
    }
}

#[derive(Debug, PartialEq)]
struct Output {
    value: u64,
    script: Script,
}

impl Output {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, value) = be_u64(input)?;
        let (input, script) = Script::parse(input)?;
        Ok((input, Output { value, script }))
    }
}

#[derive(Debug, PartialEq)]
struct Transaction {
    version: u32,
    input_count: u64,
    inputs: Vec<Input>,
    output_count: u64,
    outputs: Vec<Output>,
    lock_time: u32,
}

impl Transaction {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, version) = be_u32(input)?;
        let (input, input_count) = map_res(be_u8, |x| {
            let mut bytes = [0u8; 8];
            bytes[0] = x;
            Ok(u64::from_be_bytes(bytes))
        })(input)?;
        let (input, inputs) = many1(Input::parse)(input)?;
        let (input, output_count) = map_res(be_u8, |x| {
            let mut bytes = [0u8; 8];
            bytes[0] = x;
            Ok(u64::from_be_bytes(bytes))
        })(input)?;
        let (input, outputs) = many1(Output::parse)(input)?;
        let (input, lock_time) = be_u32(input)?;
        Ok((
            input,
            Transaction {
                version,
                input_count,
                inputs,
                output_count,
                outputs,
                lock_time,
            },
        ))
    }
}

use nom::number::complete::be_u16;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return;
    }
    let file = File::open(&args[1]).unwrap();
    let mut reader = BufReader::new(file);
    let mut data = Vec::new();
    reader.read_to_end(&mut data).unwrap();
    let (_rest, transaction) = Transaction::parse(&data).unwrap();
    println!("{:?}", transaction);
}