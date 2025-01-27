use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u16, be_u8},
    sequence::tuple,
    IResult,
};
use std::{
    env,
    fs::File,
    io::{self, Read},
};

#[derive(Debug)]
struct ModbusHeader {
    transaction_id: u16,
    protocol_id: u16,
    length: u16,
    unit_id: u8,
}

#[derive(Debug)]
enum ModbusFunction {
    ReadCoils,
    ReadDiscreteInputs,
    ReadHoldingRegisters,
    ReadInputRegisters,
    WriteSingleCoil,
    WriteSingleRegister,
    WriteMultipleCoils,
    WriteMultipleRegisters,
    Other(u8),
}

#[derive(Debug)]
struct ModbusRequest {
    header: ModbusHeader,
    function: ModbusFunction,
    data: Vec<u8>,
}

#[derive(Debug)]
struct ModbusResponse {
    header: ModbusHeader,
    function: ModbusFunction,
    data: Vec<u8>,
}

fn parse_modbus_header(input: &[u8]) -> IResult<&[u8], ModbusHeader> {
    let (input, (transaction_id, protocol_id, length, unit_id)) =
        tuple((be_u16, be_u16, be_u16, be_u8))(input)?;
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

fn parse_modbus_function(input: &[u8]) -> IResult<&[u8], ModbusFunction> {
    let (input, function_code) = be_u8(input)?;
    let function = match function_code {
        0x01 => ModbusFunction::ReadCoils,
        0x02 => ModbusFunction::ReadDiscreteInputs,
        0x03 => ModbusFunction::ReadHoldingRegisters,
        0x04 => ModbusFunction::ReadInputRegisters,
        0x05 => ModbusFunction::WriteSingleCoil,
        0x06 => ModbusFunction::WriteSingleRegister,
        0x0F => ModbusFunction::WriteMultipleCoils,
        0x10 => ModbusFunction::WriteMultipleRegisters,
        _ => ModbusFunction::Other(function_code),
    };
    Ok((input, function))
}

fn parse_modbus_request(input: &[u8]) -> IResult<&[u8], ModbusRequest> {
    let (input, header) = parse_modbus_header(input)?;
    let (input, function) = parse_modbus_function(input)?;
    let (input, data) = take(header.length - 1)(input)?;
    Ok((
        input,
        ModbusRequest {
            header,
            function,
            data: data.to_vec(),
        },
    ))
}

fn parse_modbus_response(input: &[u8]) -> IResult<&[u8], ModbusResponse> {
    let (input, header) = parse_modbus_header(input)?;
    let (input, function) = parse_modbus_function(input)?;
    let (input, data) = take(header.length - 1)(input)?;
    Ok((
        input,
        ModbusResponse {
            header,
            function,
            data: data.to_vec(),
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_modbus_request(&buffer) {
        Ok((_, request)) => println!("Parsed Modbus Request: {:?}", request),
        Err(_) => match parse_modbus_response(&buffer) {
            Ok((_, response)) => println!("Parsed Modbus Response: {:?}", response),
            Err(e) => eprintln!("Failed to parse Modbus message: {:?}", e),
        },
    }

    Ok(())
}