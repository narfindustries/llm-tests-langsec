use nom::{
    bytes::complete::{take},
    combinator::{map_res},
    multi::{many0, many1},
    number::complete::{be_u32, be_u64},
};
use std::{env, fs, io};

const VERSION: u32 = 2;
const LOCK_TIME: u32 = 0;

#[derive(Debug, PartialEq)]
enum Opcodes {
    Op0,
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
    Op1Negate,
    OpCheckLockTimeVerify,
    OpCheckSig,
    OpCheckSigVerify,
    OpIf,
    OpIfDup,
    OpNop,
    OpToAltStack,
    OpTuck,
    OpVerif,
    OpVerify,
    OpReturn,
}

impl Opcodes {
    fn from_u8(n: u8) -> Option<Self> {
        match n {
            0x00 => Some(Opcodes::Op0),
            0x51 => Some(Opcodes::Op1),
            0x52 => Some(Opcodes::Op2),
            0x53 => Some(Opcodes::Op3),
            0x54 => Some(Opcodes::Op4),
            0x55 => Some(Opcodes::Op5),
            0x56 => Some(Opcodes::Op6),
            0x57 => Some(Opcodes::Op7),
            0x58 => Some(Opcodes::Op8),
            0x59 => Some(Opcodes::Op9),
            0x5a => Some(Opcodes::Op10),
            0x5b => Some(Opcodes::Op11),
            0x5c => Some(Opcodes::Op12),
            0x5d => Some(Opcodes::Op13),
            0x5e => Some(Opcodes::Op14),
            0x5f => Some(Opcodes::Op15),
            0x60 => Some(Opcodes::Op16),
            0x4f => Some(Opcodes::Op1Negate),
            0xb1 => Some(Opcodes::OpCheckLockTimeVerify),
            0xac => Some(Opcodes::OpCheckSig),
            0xad => Some(Opcodes::OpCheckSigVerify),
            0x99 => Some(Opcodes::OpIf),
            0x73 => Some(Opcodes::OpIfDup),
            0x61 => Some(Opcodes::OpNop),
            0x6b => Some(Opcodes::OpToAltStack),
            0x7c => Some(Opcodes::OpTuck),
            0x9c => Some(Opcodes::OpVerif),
            0x69 => Some(Opcodes::OpVerify),
            0x6a => Some(Opcodes::OpReturn),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq)]
struct Script {
    opcodes: Vec<(Opcodes, Vec<u8>)>,
}

fn parse_opcode(input: &[u8]) -> nom::IResult<&[u8], (Opcodes, Vec<u8>)> {
    let (input, opcode) = take(1u8)(input)?;
    let opcode = Opcodes::from_u8(opcode[0]).unwrap();
    let (input, data_len) = take(1u8)(input)?;
    let (input, data) = take(data_len[0] as usize)(input)?;
    Ok((input, (opcode, data.to_vec())))
}

fn parse_script(input: &[u8]) -> nom::IResult<&[u8], Script> {
    let (input, opcodes) = many0(parse_opcode)(input)?;
    Ok((input, Script { opcodes }))
}

#[derive(Debug, PartialEq)]
struct TxIn {
    txid: Vec<u8>,
    vout: u32,
    script_sig: Script,
    sequence: u32,
}

fn parse_tx_in(input: &[u8]) -> nom::IResult<&[u8], TxIn> {
    let (input, txid) = take(32u8)(input)?;
    let (input, vout) = be_u32(input)?;
    let (input, script_sig) = parse_script(input)?;
    let (input, sequence) = be_u32(input)?;
    Ok((input, TxIn { txid: txid.to_vec(), vout, script_sig, sequence }))
}

#[derive(Debug, PartialEq)]
struct TxOut {
    value: u64,
    script_pub_key: Script,
}

fn parse_tx_out(input: &[u8]) -> nom::IResult<&[u8], TxOut> {
    let (input, value) = be_u64(input)?;
    let (input, script_pub_key) = parse_script(input)?;
    Ok((input, TxOut { value, script_pub_key }))
}

#[derive(Debug, PartialEq)]
struct Transaction {
    version: u32,
    tx_in: Vec<TxIn>,
    tx_out: Vec<TxOut>,
    lock_time: u32,
}

fn parse_transaction(input: &[u8]) -> nom::IResult<&[u8], Transaction> {
    let (input, version) = be_u32(input)?;
    let (input, tx_in_count) = map_res(take(1u8), |x: &[u8]| {
        let count = x[0] as usize;
        if count > 0xff {
            Err(nom::Err::Error(nom::error::ErrorKind::Count))
        } else {
            Ok(count)
        }
    })(input)?;
    let (input, tx_in) = many1(parse_tx_in)(input)?;
    if tx_in.len() != tx_in_count {
        return Err(nom::Err::Error(nom::error::ErrorKind::Count));
    }
    let (input, tx_out_count) = map_res(take(1u8), |x: &[u8]| {
        let count = x[0] as usize;
        if count > 0xff {
            Err(nom::Err::Error(nom::error::ErrorKind::Count))
        } else {
            Ok(count)
        }
    })(input)?;
    let (input, tx_out) = many1(parse_tx_out)(input)?;
    if tx_out.len() != tx_out_count {
        return Err(nom::Err::Error(nom::error::ErrorKind::Count));
    }
    let (input, lock_time) = be_u32(input)?;
    Ok((input, Transaction { version, tx_in, tx_out, lock_time }))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        return Ok(());
    }
    let input_file = &args[1];
    let input_data = fs::read(input_file)?;
    let (_rest, transaction) = parse_transaction(&input_data).unwrap();
    println!("{:?}", transaction);
    Ok(())
}