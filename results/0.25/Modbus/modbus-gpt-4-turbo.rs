use nom::{
    bytes::complete::{take, take_while_m_n},
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

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
    // Add other function codes as needed
}

#[derive(Debug)]
struct ModbusFrame {
    transaction_id: u16,
    protocol_id: u16,
    length: u16,
    unit_id: u8,
    function_code: FunctionCode,
    data: Vec<u8>,
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
        0x0F => Ok((input, FunctionCode::WriteMultipleCoils)),
        0x10 => Ok((input, FunctionCode::WriteMultipleRegisters)),
        _ => Err(nom::Err::Error((input, nom::error::ErrorKind::NoneOf))),
    }
}

fn parse_modbus_frame(input: &[u8]) -> IResult<&[u8], ModbusFrame> {
    let (input, transaction_id) = be_u16(input)?;
    let (input, protocol_id) = be_u16(input)?;
    let (input, length) = be_u16(input)?;
    let (input, unit_id) = be_u8(input)?;
    let (input, function_code) = parse_function_code(input)?;
    let (input, data) = take(length as usize - 2)(input)?; // Subtract 2 for the unit_id and function_code bytes

    Ok((
        input,
        ModbusFrame {
            transaction_id,
            protocol_id,
            length,
            unit_id,
            function_code,
            data: data.to_vec(),
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "Usage: modbus_parser <file_path>",
        ));
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_modbus_frame(&buffer) {
        Ok((_, frame)) => {
            println!("{:?}", frame);
        }
        Err(e) => {
            println!("Failed to parse Modbus frame: {:?}", e);
        }
    }

    Ok(())
}