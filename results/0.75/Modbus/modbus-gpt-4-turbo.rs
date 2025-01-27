use std::fs::File;
use std::io::{self, Read};
use std::env;
use nom::{
    IResult,
    bytes::complete::{tag, take},
    number::complete::{be_u8, be_u16},
};
    
#[derive(Debug)]
enum FunctionCode {
    ReadCoils = 0x01,
    ReadDiscreteInputs = 0x02,
    ReadHoldingRegisters = 0x03,
    ReadInputRegisters = 0x04,
    WriteSingleCoil = 0x05,
    WriteSingleRegister = 0x06,
    // Add additional function codes as needed
}

#[derive(Debug)]
struct ModbusHeader {
    transaction_id: u16,
    protocol_id: u16,
    length: u16,
    unit_id: u8,
}

#[derive(Debug)]
enum ModbusPDU {
    Request {
        function_code: FunctionCode,
        data: Vec<u8>, // Using Vec<u8> to simplify, define further per function code
    },
    Response {
        function_code: FunctionCode,
        data: Vec<u8>, // Using Vec<u8> to simplify, define further per function code
    },
}

#[derive(Debug)]
struct ModbusADU {
    header: ModbusHeader,
    pdu: ModbusPDU,
}

fn parse_function_code(input: &[u8]) -> IResult<&[u8], FunctionCode> {
    let (input, code) = be_u8(input)?;
    match code {
        0x01 => Ok((input, FunctionCode::ReadCoils)),
        0x02 => Ok((input, FunctionCode::ReadDiscreteInputs)),
        0x03 => Ok((input, FunctionCode::ReadHoldingRegisters)),
        0x04 => Ok((input, FunctionCode::ReadInputRegisters)),
        0x05 => Ok((input, FunctionCode::WriteSingleCoil)),
        0x06 => Ok((input, FunctionCode::WriteSingleRegister)),
        _ => Err(nom::Err::Error((input, nom::error::ErrorKind::Tag))),
    }
}

fn parse_header(input: &[u8]) -> IResult<&[u8], ModbusHeader> {
    let (input, transaction_id) = be_u16(input)?;
    let (input, protocol_id) = be_u16(input)?;
    let (input, length) = be_u16(input)?;
    let (input, unit_id) = be_u8(input)?;
    Ok((input, ModbusHeader { transaction_id, protocol_id, length, unit_id }))
}

fn parse_pdu(input: &[u8], function_code: FunctionCode) -> IResult<&[u8], ModbusPDU> {
    let (input, data) = take(input.len())(input)?;
    Ok((input, ModbusPDU::Request {
        function_code,
        data: data.to_vec(),
    }))
}

fn parse_modbus_adu(input: &[u8]) -> IResult<&[u8], ModbusADU> {
    let (input, header) = parse_header(input)?;
    let (input, function_code) = parse_function_code(input)?;
    let (input, pdu) = parse_pdu(input, function_code)?;
    Ok((input, ModbusADU { header, pdu }))
}

fn read_file(filename: &str) -> io::Result<Vec<u8>> {
    let mut file = File::open(filename)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;
    Ok(buffer)
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: modbus_parser <file_path>");
        return Ok(());
    }

    let filename = &args[1];
    let buffer = read_file(filename)?;

    match parse_modbus_adu(&buffer) {
        Ok((_remaining, adu)) => println!("{:?}", adu),
        Err(e) => println!("Failed to parse Modbus ADU: {:?}", e),
    }

    Ok(())
}