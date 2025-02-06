use nom::{
    bytes::complete::{take, take_while_m_n},
    combinator::{map, map_res, opt, verify},
    number::complete::{be_u16, be_u32, be_u64},
    sequence::tuple,
    IResult,
};
use std::fs::read;
use std::path::Path;

#[derive(Debug)]
struct Transaction {
    version: u32,
    inputs: Vec<TxIn>,
    outputs: Vec<TxOut>,
    lock_time: u32,
    witness: Option<Vec<Vec<Vec<u8>>>>,
}

#[derive(Debug)]
struct TxIn {
    prevout_hash: [u8; 32],
    prevout_n: u32,
    script_sig: Vec<u8>,
    sequence: u32,
}


#[derive(Debug)]
struct TxOut {
    value: u64,
    script_pubkey: Vec<u8>,
}


fn varint(input: &[u8]) -> IResult<&[u8], u64> {
    let (rest, value) = if input[0] < 0xfd {
        (input.get(1..).unwrap_or(&[]), input[0] as u64)
    } else if input[0] == 0xfd {
        let (rest, value) = be_u16(input)?;
        (rest, value as u64)
    } else if input[0] == 0xfe {
        let (rest, value) = be_u32(input)?;
        (rest, value as u64)
    } else {
        let (rest, value) = be_u64(input)?;
        (rest, value)
    };
    Ok((rest, value))
}

fn parse_txin(input: &[u8]) -> IResult<&[u8], TxIn> {
    let (input, prevout_hash) = take(32usize)(input)?;
    let (input, prevout_n) = be_u32(input)?;
    let (input, script_sig_len) = varint(input)?;
    let (input, script_sig) = take(script_sig_len as usize)(input)?;
    let (input, sequence) = be_u32(input)?;
    Ok((
        input,
        TxIn {
            prevout_hash: prevout_hash.try_into().unwrap(),
            prevout_n,
            script_sig: script_sig.to_vec(),
            sequence,
        },
    ))
}


fn parse_txout(input: &[u8]) -> IResult<&[u8], TxOut> {
    let (input, value) = be_u64(input)?;
    let (input, script_pubkey_len) = varint(input)?;
    let (input, script_pubkey) = take(script_pubkey_len as usize)(input)?;
    Ok((
        input,
        TxOut {
            value,
            script_pubkey: script_pubkey.to_vec(),
        },
    ))
}

fn parse_transaction(input: &[u8]) -> IResult<&[u8], Transaction> {
    let (input, version) = be_u32(input)?;
    let (input, num_inputs) = varint(input)?;
    let (input, inputs) = nom::multi::count(parse_txin, num_inputs as usize)(input)?;
    let (input, num_outputs) = varint(input)?;
    let (input, outputs) = nom::multi::count(parse_txout, num_outputs as usize)(input)?;
    let (input, lock_time) = be_u32(input)?;
    let (input, witness) = opt(parse_witness)(input)?;


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


fn parse_witness(input: &[u8]) -> IResult<&[u8], Vec<Vec<Vec<u8>>>> {
    let (input, num_witnesses) = varint(input)?;
    nom::multi::count(parse_witness_item, num_witnesses as usize)(input)
}

fn parse_witness_item(input: &[u8]) -> IResult<&[u8], Vec<Vec<u8>>> {
    let (input, num_items) = varint(input)?;
    nom::multi::count(parse_witness_element, num_items as usize)(input)
}

fn parse_witness_element(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, len) = varint(input)?;
    let (input, data) = take(len as usize)(input)?;
    Ok((input, data.to_vec()))

}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let bytes = read(path)?;

    match parse_transaction(&bytes) {
        Ok((_, transaction)) => println!("{:#?}", transaction),
        Err(e) => eprintln!("Parsing error: {}", e),
    }

    Ok(())
}
