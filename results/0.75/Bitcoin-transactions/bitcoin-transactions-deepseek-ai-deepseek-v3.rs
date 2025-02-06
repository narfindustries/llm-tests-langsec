use nom::{
    bytes::complete::take,
    number::complete::{le_u32, le_u64},
    sequence::tuple,
    IResult,
};
use std::{
    fs::File,
    io::Read,
    env,
};

#[derive(Debug)]
struct Transaction {
    version: u32,
    inputs: Vec<Input>,
    outputs: Vec<Output>,
    lock_time: u32,
    witness: Option<Vec<Witness>>,
}

#[derive(Debug)]
struct Input {
    previous_tx_hash: Vec<u8>,
    previous_output_index: u32,
    script_sig: Vec<u8>,
    sequence: u32,
}

#[derive(Debug)]
struct Output {
    value: u64,
    script_pub_key: Vec<u8>,
}

#[derive(Debug)]
struct Witness {
    witness_data: Vec<u8>,
}

fn parse_varint(input: &[u8]) -> IResult<&[u8], u64> {
    let (input, first_byte) = take(1usize)(input)?;
    let first_byte = first_byte[0];
    match first_byte {
        0xFD => {
            let (input, val) = take(2usize)(input)?;
            Ok((input, u16::from_le_bytes(val.try_into().unwrap()) as u64))
        }
        0xFE => {
            let (input, val) = take(4usize)(input)?;
            Ok((input, u32::from_le_bytes(val.try_into().unwrap()) as u64))
        }
        0xFF => {
            let (input, val) = take(8usize)(input)?;
            Ok((input, u64::from_le_bytes(val.try_into().unwrap())))
        }
        _ => Ok((input, first_byte as u64)),
    }
}

fn parse_input(input: &[u8]) -> IResult<&[u8], Input> {
    let (input, previous_tx_hash) = take(32usize)(input)?;
    let (input, previous_output_index) = le_u32(input)?;
    let (input, script_sig_len) = parse_varint(input)?;
    let (input, script_sig) = take(script_sig_len as usize)(input)?;
    let (input, sequence) = le_u32(input)?;

    Ok((
        input,
        Input {
            previous_tx_hash: previous_tx_hash.to_vec(),
            previous_output_index,
            script_sig: script_sig.to_vec(),
            sequence,
        },
    ))
}

fn parse_output(input: &[u8]) -> IResult<&[u8], Output> {
    let (input, value) = le_u64(input)?;
    let (input, script_pub_key_len) = parse_varint(input)?;
    let (input, script_pub_key) = take(script_pub_key_len as usize)(input)?;

    Ok((
        input,
        Output {
            value,
            script_pub_key: script_pub_key.to_vec(),
        },
    ))
}

fn parse_witness(input: &[u8]) -> IResult<&[u8], Witness> {
    let (input, witness_data_len) = parse_varint(input)?;
    let (input, witness_data) = take(witness_data_len as usize)(input)?;

    Ok((
        input,
        Witness {
            witness_data: witness_data.to_vec(),
        },
    ))
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    let (input, version) = le_u32(input)?;
    let (input, marker) = take(1usize)(input)?;
    let (input, flag) = take(1usize)(input)?;

    let has_witness = marker[0] == 0x00 && flag[0] == 0x01;

    let (input, input_count) = parse_varint(input)?;
    let (input, inputs) = nom::multi::fold_many_m_n(
        input_count as usize,
        input_count as usize,
        parse_input,
        Vec::new,
        |mut acc, item| {
            acc.push(item);
            acc
        },
    )(input)?;

    let (input, output_count) = parse_varint(input)?;
    let (input, outputs) = nom::multi::fold_many_m_n(
        output_count as usize,
        output_count as usize,
        parse_output,
        Vec::new,
        |mut acc, item| {
            acc.push(item);
            acc
        },
    )(input)?;

    let (input, witness) = if has_witness {
        let (input, witness_count) = parse_varint(input)?;
        let (input, witness) = nom::multi::fold_many_m_n(
            witness_count as usize,
            witness_count as usize,
            parse_witness,
            Vec::new,
            |mut acc, item| {
                acc.push(item);
                acc
            },
        )(input)?;
        (input, Some(witness))
    } else {
        (input, None)
    };

    let (input, lock_time) = le_u32(input)?;

    Ok((
        input,
        Transaction {
            version,
            inputs,
            outputs,
            lock_time,
            witness,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <binary file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_transaction(&buffer) {
        Ok((_, transaction)) => println!("{:#?}", transaction),
        Err(e) => eprintln!("Failed to parse transaction: {:?}", e),
    }
}