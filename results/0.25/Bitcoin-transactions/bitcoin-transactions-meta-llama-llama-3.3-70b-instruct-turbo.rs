use nom::{
    bytes::complete::{take, take_while_m_n},
    multi::{many_till},
    number::complete::{be_u16, be_u32, be_u64, be_u8},
    IResult,
};
use nom::error::ParseError;
use std::env;
use std::fs::File;
use std::io::{Read};
use std::path::Path;

#[derive(Debug)]
enum Opcodes {
    Op0,
    Op1(u8),
    OpPushdata1(u8),
    OpPushdata2(u16),
    OpPushdata4(u32),
    OpChecksig,
    OpChecksigverify,
    OpDup,
    OpHash160,
    OpEqual,
    OpEqualverify,
}

impl Opcodes {
    fn parse(input: &[u8]) -> IResult<&[u8], Opcodes> {
        let (input, opcode) = be_u8(input)?;
        match opcode {
            0x00 => Ok((input, Opcodes::Op0)),
            0x01..=0x10 => Ok((input, Opcodes::Op1(opcode - 0x01))),
            0x4c => {
                let (input, length) = be_u8(input)?;
                Ok((input, Opcodes::OpPushdata1(length)))
            }
            0x4d => {
                let (input, length) = be_u16(input)?;
                Ok((input, Opcodes::OpPushdata2(length)))
            }
            0x4e => {
                let (input, length) = be_u32(input)?;
                Ok((input, Opcodes::OpPushdata4(length)))
            }
            0xac => Ok((input, Opcodes::OpChecksig)),
            0xad => Ok((input, Opcodes::OpChecksigverify)),
            0x76 => Ok((input, Opcodes::OpDup)),
            0xa9 => Ok((input, Opcodes::OpHash160)),
            0x87 => Ok((input, Opcodes::OpEqual)),
            0x88 => Ok((input, Opcodes::OpEqualverify)),
            _ => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::NonEmpty))),
        }
    }
}

#[derive(Debug)]
struct Script {
    opcodes: Vec<Opcodes>,
}

impl Script {
    fn parse(input: &[u8]) -> IResult<&[u8], Script> {
        let (input, opcodes) = many_till(
            Opcodes::parse,
            take_while_m_n(0, 1, |x| x == 0x00),
        )(input)?;
        Ok((input, Script { opcodes: opcodes.0 }))
    }
}

#[derive(Debug)]
struct TxIn {
    previous_output_hash: [u8; 32],
    previous_output_index: u32,
    script: Script,
    sequence: u32,
}

impl TxIn {
    fn parse(input: &[u8]) -> IResult<&[u8], TxIn> {
        let (input, previous_output_hash) = take(32usize)(input)?;
        let (input, previous_output_index) = be_u32(input)?;
        let (input, script) = Script::parse(input)?;
        let (input, sequence) = be_u32(input)?;
        Ok((input, TxIn {
            previous_output_hash: previous_output_hash.try_into().unwrap(),
            previous_output_index,
            script,
            sequence,
        }))
    }
}

#[derive(Debug)]
struct TxOut {
    value: u64,
    script: Script,
}

impl TxOut {
    fn parse(input: &[u8]) -> IResult<&[u8], TxOut> {
        let (input, value) = be_u64(input)?;
        let (input, script) = Script::parse(input)?;
        Ok((input, TxOut { value, script }))
    }
}

#[derive(Debug)]
struct Transaction {
    version: u32,
    tx_in_count: u16,
    tx_in: Vec<TxIn>,
    tx_out_count: u16,
    tx_out: Vec<TxOut>,
    lock_time: u32,
}

impl Transaction {
    fn parse(input: &[u8]) -> IResult<&[u8], Transaction> {
        let (input, version) = be_u32(input)?;
        let (input, tx_in_count) = be_u16(input)?;
        let (input, tx_in) = many_till(
            TxIn::parse,
            take_while_m_n(0, 1, |x| x == 0x00),
        )(input)?;
        let (input, tx_out_count) = be_u16(input)?;
        let (input, tx_out) = many_till(
            TxOut::parse,
            take_while_m_n(0, 1, |x| x == 0x00),
        )(input)?;
        let (input, lock_time) = be_u32(input)?;
        Ok((input, Transaction {
            version,
            tx_in_count,
            tx_in: tx_in.0,
            tx_out_count,
            tx_out: tx_out.0,
            lock_time,
        }))
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        return;
    }
    let input_file = &args[1];
    let path = Path::new(input_file);
    let mut file = match File::open(path) {
        Ok(file) => file,
        Err(_) => {
            eprintln!("Error opening file: {}", input_file);
            return;
        }
    };
    let mut input = Vec::new();
    match file.read_to_end(&mut input) {
        Ok(_) => (),
        Err(_) => {
            eprintln!("Error reading file: {}", input_file);
            return;
        }
    }
    match Transaction::parse(&input) {
        Ok((_, transaction)) => println!("{:?}", transaction),
        Err(err) => eprintln!("Error parsing transaction: {:?}", err),
    }
}