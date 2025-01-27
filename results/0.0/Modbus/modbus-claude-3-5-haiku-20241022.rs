use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    error::ErrorKind,
    multi::many0,
    sequence::tuple,
    IResult,
    number::complete::{be_u8, be_u16, be_u32},
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct ModbusFrame {
    slave_address: u8,
    function_code: u8,
    data: Vec<u8>,
    crc: Option<u16>,
}

fn parse_modbus_frame(input: &[u8]) -> IResult<&[u8], ModbusFrame> {
    let (input, slave_address) = be_u8(input)?;
    let (input, function_code) = be_u8(input)?;
    
    let (input, data) = match function_code {
        0x01 | 0x02 | 0x03 | 0x04 => parse_read_data(input)?,
        0x05 | 0x06 => parse_write_single_data(input)?,
        0x0F | 0x10 => parse_write_multiple_data(input)?,
        _ => (input, Vec::new()),
    };

    let (input, crc) = opt(be_u16)(input)?;

    Ok((input, ModbusFrame {
        slave_address,
        function_code,
        data,
        crc,
    }))
}

fn parse_read_data(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, byte_count) = be_u8(input)?;
    let (input, data) = take(byte_count)(input)?;
    Ok((input, data.to_vec()))
}

fn parse_write_single_data(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, register_address) = be_u16(input)?;
    let (input, register_value) = be_u16(input)?;
    Ok((input, vec![
        (register_address >> 8) as u8,
        register_address as u8,
        (register_value >> 8) as u8,
        register_value as u8,
    ]))
}

fn parse_write_multiple_data(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, start_address) = be_u16(input)?;
    let (input, quantity) = be_u16(input)?;
    let (input, byte_count) = be_u8(input)?;
    let (input, data) = take(byte_count)(input)?;
    Ok((input, [
        vec![(start_address >> 8) as u8, start_address as u8],
        vec![(quantity >> 8) as u8, quantity as u8],
        vec![byte_count],
        data.to_vec()
    ].concat()))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_modbus_frame(&buffer) {
        Ok((_, frame)) => {
            println!("Parsed Modbus Frame: {:?}", frame);
            Ok(())
        }
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1);
        }
    }
}