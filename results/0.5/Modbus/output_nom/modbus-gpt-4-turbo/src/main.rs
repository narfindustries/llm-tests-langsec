use nom::{
    bytes::complete::{take, take_while_m_n},
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug, PartialEq)]
struct ModbusTCPFrame {
    transaction_id: u16,
    protocol_id: u16,
    length: u16,
    unit_id: u8,
    function_code: u8,
    data: Vec<u8>,
}

#[derive(Debug, PartialEq)]
struct ModbusRTUFrame {
    address: u8,
    function_code: u8,
    data: Vec<u8>,
    crc: u16,
}

fn parse_modbus_tcp(input: &[u8]) -> IResult<&[u8], ModbusTCPFrame> {
    let (input, transaction_id) = be_u16(input)?;
    let (input, protocol_id) = be_u16(input)?;
    let (input, length) = be_u16(input)?;
    let (input, unit_id) = be_u8(input)?;
    let (input, function_code) = be_u8(input)?;
    let (input, data) = take(length - 2)(input)?;

    Ok((
        input,
        ModbusTCPFrame {
            transaction_id,
            protocol_id,
            length,
            unit_id,
            function_code,
            data: data.to_vec(),
        },
    ))
}

fn parse_modbus_rtu(input: &[u8]) -> IResult<&[u8], ModbusRTUFrame> {
    let (input, address) = be_u8(input)?;
    let (input, function_code) = be_u8(input)?;
    let (input, data) = take_while_m_n(0, input.len() - 2, |_| true)(input)?;
    let (input, crc) = be_u16(input)?;

    Ok((
        input,
        ModbusRTUFrame {
            address,
            function_code,
            data: data.to_vec(),
            crc,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <modbus_binary_file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_modbus_tcp(&buffer) {
        Ok((_, frame)) => println!("{:?}", frame),
        Err(_) => match parse_modbus_rtu(&buffer) {
            Ok((_, frame)) => println!("{:?}", frame),
            Err(e) => eprintln!("Failed to parse Modbus frame: {:?}", e),
        },
    }

    Ok(())
}