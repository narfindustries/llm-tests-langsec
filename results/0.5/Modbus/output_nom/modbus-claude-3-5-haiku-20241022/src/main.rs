use nom::{
    bytes::complete::{take, tag},
    combinator::{map, opt},
    multi::{count, many0},
    number::complete::{le_u8, le_u16, le_u32},
    sequence::{tuple, preceded},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
enum FunctionCode {
    ReadCoils = 0x01,
    ReadDiscreteInputs = 0x02,
    ReadHoldingRegisters = 0x03,
    ReadInputRegisters = 0x04,
    WriteSingleCoil = 0x05,
    WriteSingleRegister = 0x06,
    WriteMultipleCoils = 0x0F,
    WriteMultipleRegisters = 0x10,
    ReadWriteMultipleRegisters = 0x17,
}

#[derive(Debug)]
struct ModbusADU {
    server_address: u8,
    function_code: FunctionCode,
    data: Vec<u8>,
    crc: u16,
}

fn parse_function_code(input: &[u8]) -> IResult<&[u8], FunctionCode> {
    map(le_u8, |code| match code {
        0x01 => FunctionCode::ReadCoils,
        0x02 => FunctionCode::ReadDiscreteInputs,
        0x03 => FunctionCode::ReadHoldingRegisters,
        0x04 => FunctionCode::ReadInputRegisters,
        0x05 => FunctionCode::WriteSingleCoil,
        0x06 => FunctionCode::WriteSingleRegister,
        0x0F => FunctionCode::WriteMultipleCoils,
        0x10 => FunctionCode::WriteMultipleRegisters,
        0x17 => FunctionCode::ReadWriteMultipleRegisters,
        _ => panic!("Unknown function code"),
    })(input)
}

fn parse_modbus_adu(input: &[u8]) -> IResult<&[u8], ModbusADU> {
    map(
        tuple((
            le_u8,
            parse_function_code,
            many0(le_u8),
            le_u16,
        )),
        |(server_address, function_code, data, crc)| ModbusADU {
            server_address,
            function_code,
            data,
            crc,
        }
    )(input)
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <modbus_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_modbus_adu(&buffer) {
        Ok((_, parsed_adu)) => {
            println!("Parsed Modbus ADU: {:?}", parsed_adu);
            Ok(())
        }
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1)
        }
    }
}