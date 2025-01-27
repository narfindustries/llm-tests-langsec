use nom::{
    bytes::complete::take,
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
enum ModbusFunction {
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
struct ModbusHeader {
    transaction_id: u16,
    protocol_id: u16,
    length: u16,
    unit_id: u8,
}

#[derive(Debug)]
struct ModbusRequest {
    header: ModbusHeader,
    function_code: ModbusFunction,
    data: Vec<u8>,
}

fn parse_modbus_header(input: &[u8]) -> IResult<&[u8], ModbusHeader> {
    let (input, transaction_id) = be_u16(input)?;
    let (input, protocol_id) = be_u16(input)?;
    let (input, length) = be_u16(input)?;
    let (input, unit_id) = be_u8(input)?;

    Ok((
        input,
        ModbusHeader {
            transaction_id,
            protocol_id,
            length,
            unit_id,
        },
    ))
}

fn parse_modbus_request(input: &[u8]) -> IResult<&[u8], ModbusRequest> {
    let (input, header) = parse_modbus_header(input)?;
    let (input, function_code) = be_u8(input)?;
    let function_code = match function_code {
        0x01 => ModbusFunction::ReadCoils,
        0x02 => ModbusFunction::ReadDiscreteInputs,
        0x03 => ModbusFunction::ReadHoldingRegisters,
        0x04 => ModbusFunction::ReadInputRegisters,
        0x05 => ModbusFunction::WriteSingleCoil,
        0x06 => ModbusFunction::WriteSingleRegister,
        0x0F => ModbusFunction::WriteMultipleCoils,
        0x10 => ModbusFunction::WriteMultipleRegisters,
        _ => return Err(nom::Err::Error((input, nom::error::ErrorKind::NoneOf))),
    };

    let (input, data) = take(header.length - 2)(input)?;

    Ok((
        input,
        ModbusRequest {
            header,
            function_code,
            data: data.to_vec(),
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <MODBUS_REQUEST_FILE>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_modbus_request(&buffer) {
        Ok((_, request)) => println!("{:?}", request),
        Err(e) => println!("Failed to parse Modbus request: {:?}", e),
    }

    Ok(())
}