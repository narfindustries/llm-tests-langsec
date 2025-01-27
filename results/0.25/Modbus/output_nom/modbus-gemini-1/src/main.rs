use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt},
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
enum ModbusFunction {
    ReadCoils(u16, u16),
    ReadDiscreteInputs(u16, u16),
    ReadHoldingRegisters(u16, u16),
    ReadInputRegisters(u16, u16),
    WriteSingleCoil(u16, bool),
    WriteSingleRegister(u16, u16),
    WriteMultipleCoils(u16, u16, Vec<u8>),
    WriteMultipleRegisters(u16, u16, Vec<u8>),
    ReadExceptionStatus,
    // Add other functions as needed...
}


fn parse_modbus_pdu(input: &[u8]) -> IResult<&[u8], ModbusFunction> {
    let (input, function_code) = be_u8(input)?;

    match function_code {
        1 => {
            let (input, start_address) = be_u16(input)?;
            let (input, quantity) = be_u16(input)?;
            Ok((input, ModbusFunction::ReadCoils(start_address, quantity)))
        }
        2 => {
            let (input, start_address) = be_u16(input)?;
            let (input, quantity) = be_u16(input)?;
            Ok((input, ModbusFunction::ReadDiscreteInputs(start_address, quantity)))
        }
        3 => {
            let (input, start_address) = be_u16(input)?;
            let (input, quantity) = be_u16(input)?;
            Ok((input, ModbusFunction::ReadHoldingRegisters(start_address, quantity)))
        }
        4 => {
            let (input, start_address) = be_u16(input)?;
            let (input, quantity) = be_u16(input)?;
            Ok((input, ModbusFunction::ReadInputRegisters(start_address, quantity)))
        }
        5 => {
            let (input, output_address) = be_u16(input)?;
            let (input, value) = be_u16(input)?;
            Ok((input, ModbusFunction::WriteSingleCoil(output_address, value > 0)))
        }
        6 => {
            let (input, register_address) = be_u16(input)?;
            let (input, value) = be_u16(input)?;
            Ok((input, ModbusFunction::WriteSingleRegister(register_address, value)))
        }
        15 => {
            let (input, start_address) = be_u16(input)?;
            let (input, quantity) = be_u16(input)?;
            let (input, byte_count) = be_u8(input)?;
            let (input, data) = take(byte_count as usize)(input)?;
            Ok((input, ModbusFunction::WriteMultipleCoils(start_address, quantity, data.to_vec())))
        }
        16 => {
            let (input, start_address) = be_u16(input)?;
            let (input, quantity) = be_u16(input)?;
            let (input, byte_count) = be_u8(input)?;
            let (input, data) = take(byte_count as usize)(input)?;
            Ok((input, ModbusFunction::WriteMultipleRegisters(start_address, quantity, data.to_vec())))
        }
        // Add other functions as needed...
        _ => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = match File::open(filename) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Error opening file {}: {}", filename, err);
            return;
        }
    };

    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Ok(_) => (),
        Err(err) => {
            eprintln!("Error reading file: {}", err);
            return;
        }
    };

    match parse_modbus_pdu(&buffer) {
        Ok((_, pdu)) => println!("Parsed Modbus PDU: {:?}", pdu),
        Err(e) => println!("Error parsing Modbus PDU: {:?}", e),
    }
}
