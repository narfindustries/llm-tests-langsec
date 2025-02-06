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
    FileRecordRead = 0x14,
    FileRecordWrite = 0x15,
    EncapsulatedInterfaceTransport = 0x2B,
}

#[derive(Debug)]
struct ModbusFrame {
    slave_address: u8,
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
        0x14 => FunctionCode::FileRecordRead,
        0x15 => FunctionCode::FileRecordWrite,
        0x2B => FunctionCode::EncapsulatedInterfaceTransport,
        _ => panic!("Invalid function code"),
    })(input)
}

fn parse_modbus_frame(input: &[u8]) -> IResult<&[u8], ModbusFrame> {
    let (input, slave_address) = le_u8(input)?;
    let (input, function_code) = parse_function_code(input)?;
    let (input, data) = many0(le_u8)(input)?;
    let (input, crc) = le_u16(input)?;

    Ok((input, ModbusFrame {
        slave_address,
        function_code,
        data,
        crc,
    }))
}

fn main() -> std::io::Result<()> {
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
        }
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}