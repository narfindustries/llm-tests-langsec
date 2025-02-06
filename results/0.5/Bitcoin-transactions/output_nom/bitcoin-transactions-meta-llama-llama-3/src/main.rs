use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    error::ErrorKind,
    multi::{many_till},
    number::complete::{be_u32, be_u64, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
enum ScriptOp {
    Op0,
    OpPushData(Vec<u8>),
    Op1Negate,
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
    ToAltStack,
    FromAltStack,
    If,
    IfDup,
    IfElse,
    Verify,
    ToMemory,
    FromMemory,
    ToAltMemory,
    FromAltMemory,
    Xor,
    Equal,
    EqualVerify,
    Add1,
    Subtract1,
    Multiply2,
    Invert,
    LogicalAnd,
    LogicalOr,
    LogicalNot,
    Hash1,
    Hash2,
    Hash256,
    Hash512,
    CheckSig,
    CheckSigVerify,
    CheckMultiSig,
    CheckMultiSigVerify,
    CheckLockTimeVerify,
    CheckSequenceVerify,
    InvalidOp,
}

fn script_op(input: &[u8]) -> IResult<&[u8], ScriptOp> {
    map_res(be_u8, |op_code| match op_code {
        0x00 => Ok(ScriptOp::Op0),
        0x01 => Ok(ScriptOp::Op1),
        0x02 => Ok(ScriptOp::Op2),
        0x03 => Ok(ScriptOp::Op3),
        0x04 => Ok(ScriptOp::Op4),
        0x05 => Ok(ScriptOp::Op5),
        0x06 => Ok(ScriptOp::Op6),
        0x07 => Ok(ScriptOp::Op7),
        0x08 => Ok(ScriptOp::Op8),
        0x09 => Ok(ScriptOp::Op9),
        0x0a => Ok(ScriptOp::Op10),
        0x0b => Ok(ScriptOp::Op11),
        0x0c => Ok(ScriptOp::Op12),
        0x0d => Ok(ScriptOp::Op13),
        0x0e => Ok(ScriptOp::Op14),
        0x0f => Ok(ScriptOp::Op15),
        0x10 => Ok(ScriptOp::Op16),
        0x6b => Ok(ScriptOp::ToAltStack),
        0x6c => Ok(ScriptOp::FromAltStack),
        0x99 => Ok(ScriptOp::If),
        0x9a => Ok(ScriptOp::IfDup),
        0x9b => Ok(ScriptOp::IfElse),
        0x9c => Ok(ScriptOp::Verify),
        0x9d => Ok(ScriptOp::ToMemory),
        0x9e => Ok(ScriptOp::FromMemory),
        0x9f => Ok(ScriptOp::ToAltMemory),
        0xa0 => Ok(ScriptOp::FromAltMemory),
        0xa1 => Ok(ScriptOp::Xor),
        0xa2 => Ok(ScriptOp::Equal),
        0xa3 => Ok(ScriptOp::EqualVerify),
        0xa4 => Ok(ScriptOp::Add1),
        0xa5 => Ok(ScriptOp::Subtract1),
        0xa6 => Ok(ScriptOp::Multiply2),
        0xa7 => Ok(ScriptOp::Invert),
        0xa8 => Ok(ScriptOp::LogicalAnd),
        0xa9 => Ok(ScriptOp::LogicalOr),
        0xaa => Ok(ScriptOp::LogicalNot),
        0xab => Ok(ScriptOp::Hash1),
        0xac => Ok(ScriptOp::Hash2),
        0xad => Ok(ScriptOp::Hash256),
        0xae => Ok(ScriptOp::Hash512),
        0xb0 => Ok(ScriptOp::CheckSig),
        0xb1 => Ok(ScriptOp::CheckSigVerify),
        0xb2 => Ok(ScriptOp::CheckMultiSig),
        0xb3 => Ok(ScriptOp::CheckMultiSigVerify),
        0xb4 => Ok(ScriptOp::CheckLockTimeVerify),
        0xb5 => Ok(ScriptOp::CheckSequenceVerify),
        _ => Err(nom::Err::Error((input, ErrorKind::AlphaNumeric))),
    })(input)
}

fn script(input: &[u8]) -> IResult<&[u8], Vec<ScriptOp>> {
    let (input, _) = tag(&[0x00])(input)?;
    let (input, (script_ops, _)) = many_till(script_op, tag(&[0xff]))(input)?;
    Ok((input, script_ops))
}

#[derive(Debug)]
struct TxIn {
    txid: Vec<u8>,
    vout: u32,
    script_sig: Vec<ScriptOp>,
    sequence: u32,
}

fn tx_in(input: &[u8]) -> IResult<&[u8], TxIn> {
    map(
        tuple((
            take(32u8),
            be_u32,
            preceded(tag(&[0x00]), script),
            be_u32,
        )),
        |(txid, vout, script_sig, sequence)| TxIn {
            txid: txid.to_vec(),
            vout,
            script_sig,
            sequence,
        },
    )(input)
}

#[derive(Debug)]
struct TxOut {
    value: u64,
    script_pub_key: Vec<ScriptOp>,
}

fn tx_out(input: &[u8]) -> IResult<&[u8], TxOut> {
    map(
        tuple((
            be_u64,
            preceded(tag(&[0x00]), script),
        )),
        |(value, script_pub_key)| TxOut { value, script_pub_key },
    )(input)
}

#[derive(Debug)]
struct Transaction {
    tx_in_count: u64,
    tx_in: Vec<TxIn>,
    tx_out_count: u64,
    tx_out: Vec<TxOut>,
    lock_time: u32,
}

fn transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    map(
        tuple((
            map_res(be_u32, |tx_in_count| {
                if tx_in_count > 0xff {
                    Ok(tx_in_count as u64)
                } else {
                    Err(nom::Err::Error((input, ErrorKind::AlphaNumeric)))
                }
            }),
            many_till(tx_in, tag(&[0xff])),
            map_res(be_u32, |tx_out_count| {
                if tx_out_count > 0xff {
                    Ok(tx_out_count as u64)
                } else {
                    Err(nom::Err::Error((input, ErrorKind::AlphaNumeric)))
                }
            }),
            many_till(tx_out, tag(&[0xff])),
            be_u32,
        )),
        |(tx_in_count, (tx_in, _), tx_out_count, (tx_out, _), lock_time)| Transaction {
            tx_in_count,
            tx_in,
            tx_out_count,
            tx_out,
            lock_time,
        },
    )(input)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).unwrap();
    let mut input = Vec::new();
    file.read_to_end(&mut input).unwrap();

    match transaction(&input) {
        Ok((remaining, transaction)) => {
            println!("Transaction: {:?}", transaction);
            if !remaining.is_empty() {
                println!("Warning: Remaining bytes: {:?}", remaining);
            }
        }
        Err(err) => {
            println!("Error: {:?}", err);
        }
    }
}