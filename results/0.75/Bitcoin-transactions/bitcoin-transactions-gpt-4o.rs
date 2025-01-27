use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_opt, map_res},
    multi::{length_data, many0, many1},
    number::complete::{le_i32, le_u32, le_u64, le_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct BitcoinTransaction {
    version: u32,
    flag: Option<u16>,
    inputs: Vec<TxInput>,
    outputs: Vec<TxOutput>,
    witnesses: Option<Vec<Vec<Vec<u8>>>>,
    lock_time: u32,
}

#[derive(Debug)]
struct TxInput {
    previous_output: OutPoint,
    script_sig: Vec<u8>,
    sequence: u32,
}

#[derive(Debug)]
struct OutPoint {
    hash: [u8; 32],
    index: u32,
}

#[derive(Debug)]
struct TxOutput {
    value: u64,
    script_pubkey: Vec<u8>,
}

fn var_int(input: &[u8]) -> IResult<&[u8], u64> {
    let (input, first_byte) = le_u8(input)?;
    match first_byte {
        0xFD => map(le_u16, |x| x as u64)(input),
        0xFE => map(le_u32, |x| x as u64)(input),
        0xFF => le_u64(input),
        x => Ok((input, x as u64)),
    }
}

fn parse_out_point(input: &[u8]) -> IResult<&[u8], OutPoint> {
    let (input, (hash, index)) = tuple((take(32usize), le_u32))(input)?;
    let hash_array: [u8; 32] = hash.try_into().unwrap();
    Ok((input, OutPoint { hash: hash_array, index }))
}

fn parse_tx_input(input: &[u8]) -> IResult<&[u8], TxInput> {
    let (input, (previous_output, script_sig, sequence)) = tuple((
        parse_out_point,
        length_data(var_int),
        le_u32,
    ))(input)?;
    Ok((input, TxInput {
        previous_output,
        script_sig: script_sig.to_vec(),
        sequence,
    }))
}

fn parse_tx_output(input: &[u8]) -> IResult<&[u8], TxOutput> {
    let (input, (value, script_pubkey)) = tuple((le_u64, length_data(var_int)))(input)?;
    Ok((input, TxOutput {
        value,
        script_pubkey: script_pubkey.to_vec(),
    }))
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], BitcoinTransaction> {
    let (input, version) = le_u32(input)?;
    let mut input_mut = input;
    let mut flag = None;
    let segwit = if let Ok((new_input, marker)) = le_u8(input_mut) {
        if marker == 0 {
            let (new_input, new_flag) = le_u8(new_input)?;
            if new_flag == 1 {
                flag = Some(new_flag as u16);
                input_mut = new_input;
                true
            } else {
                false
            }
        } else {
            false
        }
    } else {
        false
    };

    let (input, inputs) = length_count(var_int, parse_tx_input)(input_mut)?;
    let (input, outputs) = length_count(var_int, parse_tx_output)(input)?;

    let mut witnesses = None;
    let input = if segwit {
        let (input, witness_data) = many_m_n(inputs.len(), inputs.len(), length_count(var_int, length_data(var_int)))(input)?;
        witnesses = Some(witness_data);
        input
    } else {
        input
    };

    let (input, lock_time) = le_u32(input)?;
    Ok((input, BitcoinTransaction {
        version,
        flag,
        inputs,
        outputs,
        witnesses,
        lock_time,
    }))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        return Ok(());
    }
    
    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_transaction(&buffer) {
        Ok((_, transaction)) => {
            println!("{:#?}", transaction);
        }
        Err(e) => {
            eprintln!("Failed to parse transaction: {:?}", e);
        }
    }
    
    Ok(())
}