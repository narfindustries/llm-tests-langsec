use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::{length_data, many_till},
    number::complete::{be_u32, be_u64, be_i64},
    sequence::{tuple, preceded},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
    path::Path,
};
use std::convert::TryFrom;

#[derive(Debug)]
enum VarInt {
    U64(u64),
}

impl VarInt {
    fn from_u64(n: u64) -> VarInt {
        VarInt::U64(n)
    }
}

fn var_int(input: &[u8]) -> IResult<&[u8], VarInt> {
    let (input, first_byte) = take(1u8)(input)?;
    let first_byte = first_byte[0];
    if first_byte < 0xFD {
        let n = u64::from(first_byte);
        Ok((input, VarInt::from_u64(n)))
    } else if first_byte == 0xFD {
        let (input, n) = be_u16(input)?;
        Ok((input, VarInt::from_u64(u64::try_from(n).unwrap())))
    } else if first_byte == 0xFE {
        let (input, n) = be_u32(input)?;
        Ok((input, VarInt::from_u64(u64::try_from(n).unwrap())))
    } else {
        let (input, n) = be_u64(input)?;
        Ok((input, VarInt::from_u64(n)))
    }
}

#[derive(Debug)]
struct OutPoint {
    tx_id: [u8; 32],
    vout: u32,
}

fn out_point(input: &[u8]) -> IResult<&[u8], OutPoint> {
    let (input, tx_id) = take(32u8)(input)?;
    let (input, vout) = be_u32(input)?;
    let tx_id: [u8; 32] = tx_id.try_into().unwrap();
    Ok((input, OutPoint { tx_id, vout }))
}

#[derive(Debug)]
enum ScriptType {
    P2PKH,
    P2SH,
    P2WPKH,
    P2WSH,
}

#[derive(Debug)]
struct Input {
    out_point: OutPoint,
    script_sig: Vec<u8>,
    sequence: u32,
}

fn input(input: &[u8]) -> IResult<&[u8], Input> {
    let (input, out_point) = out_point(input)?;
    let (input, script_sig_len) = var_int(input)?;
    let script_sig_len = match script_sig_len {
        VarInt::U64(n) => n as usize,
    };
    let (input, script_sig) = take(script_sig_len)(input)?;
    let (input, sequence) = be_u32(input)?;
    Ok((input, Input { out_point, script_sig: script_sig.to_vec(), sequence }))
}

#[derive(Debug)]
struct Output {
    value: i64,
    script_pub_key: Vec<u8>,
}

fn output(input: &[u8]) -> IResult<&[u8], Output> {
    let (input, value) = be_i64(input)?;
    let (input, script_pub_key_len) = var_int(input)?;
    let script_pub_key_len = match script_pub_key_len {
        VarInt::U64(n) => n as usize,
    };
    let (input, script_pub_key) = take(script_pub_key_len)(input)?;
    Ok((input, Output { value, script_pub_key: script_pub_key.to_vec() }))
}

#[derive(Debug)]
struct Tx {
    version: i32,
    tx_in_count: VarInt,
    tx_ins: Vec<Input>,
    tx_out_count: VarInt,
    tx_outs: Vec<Output>,
    lock_time: u32,
}

fn tx(input: &[u8]) -> IResult<&[u8], Tx> {
    let (input, version) = be_i32(input)?;
    let (input, tx_in_count) = var_int(input)?;
    let tx_in_count = match tx_in_count {
        VarInt::U64(n) => n as usize,
    };
    let (input, tx_ins) = many_till(input, tx_in_count, input)(input)?;
    let (input, tx_out_count) = var_int(input)?;
    let tx_out_count = match tx_out_count {
        VarInt::U64(n) => n as usize,
    };
    let (input, tx_outs) = many_till(input, tx_out_count, input)(input)?;
    let (input, lock_time) = be_u32(input)?;
    Ok((input, Tx { version, tx_in_count, tx_ins, tx_out_count, tx_outs, lock_time }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return;
    }
    let path = Path::new(&args[1]);
    let file = File::open(path).unwrap();
    let reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input).unwrap();
    let (_rest, tx) = tx(&input).unwrap();
    println!("{:?}", tx);
}