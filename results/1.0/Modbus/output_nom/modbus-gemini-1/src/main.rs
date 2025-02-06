use nom::{
    bytes::complete::{take, take_until},
    combinator::{map, map_res},
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::env;
use std::fs;

// Modbus function codes
#[derive(Debug, PartialEq)]
enum FunctionCode {
    ReadCoils(u16),
    ReadDiscreteInputs(u16),
    ReadHoldingRegisters(u16),
    ReadInputRegisters(u16),
    WriteSingleCoil(u16, bool),
    WriteSingleRegister(u16, u16),
    WriteMultipleCoils(u16, u16),
    WriteMultipleRegisters(u16, u16),
    // Add other function codes as needed...
    Other(u8),
}

// Modbus exception codes
#[derive(Debug, PartialEq)]
enum ExceptionCode {
    IllegalFunction,
    IllegalDataAddress,
    IllegalDataValue,
    SlaveDeviceFailure,
    // Add other exception codes as needed...
    Other(u8),
}

// Parser for Modbus PDU
fn parse_modbus_pdu(input: &[u8]) -> IResult<&[u8], FunctionCode> {
    let (input, function_code) = be_u8(input)?;

    match function_code {
        1 => {
            let (input, start_address) = be_u16(input)?;
            let (input, quantity) = be_u16(input)?;
            Ok((input, FunctionCode::ReadCoils(quantity)))
        }
        2 => {
            let (input, start_address) = be_u16(input)?;
            let (input, quantity) = be_u16(input)?;
            Ok((input, FunctionCode::ReadDiscreteInputs(quantity)))
        }
        3 => {
            let (input, start_address) = be_u16(input)?;
            let (input, quantity) = be_u16(input)?;
            Ok((input, FunctionCode::ReadHoldingRegisters(quantity)))
        }
        4 => {
            let (input, start_address) = be_u16(input)?;
            let (input, quantity) = be_u16(input)?;
            Ok((input, FunctionCode::ReadInputRegisters(quantity)))

        }
        5 => {
            let (input, start_address) = be_u16(input)?;
            let (input, value) = be_u16(input)?;
            Ok((input, FunctionCode::WriteSingleCoil(start_address, value !=0)))
        }
        6 => {
            let (input, start_address) = be_u16(input)?;
            let (input, value) = be_u16(input)?;
            Ok((input, FunctionCode::WriteSingleRegister(start_address, value)))
        }
        15 => {
            let (input, start_address) = be_u16(input)?;
            let (input, quantity) = be_u16(input)?;
            let (input, byte_count) = be_u8(input)?;
            // Process coils based on byte_count
            Ok((input, FunctionCode::WriteMultipleCoils(start_address, quantity)))
        }
        16 => {
            let (input, start_address) = be_u16(input)?;
            let (input, quantity) = be_u16(input)?;
            let (input, byte_count) = be_u8(input)?;
            // Process registers based on byte_count
            Ok((input, FunctionCode::WriteMultipleRegisters(start_address, quantity)))
        }
        _ => Ok((input, FunctionCode::Other(function_code))),
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let contents = fs::read(filename).expect("Failed to read file");

    match parse_modbus_pdu(&contents) {
        Ok((_, pdu)) => println!("Parsed Modbus PDU: {:?}", pdu),
        Err(e) => println!("Failed to parse Modbus PDU: {:?}", e),
    }
}
