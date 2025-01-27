use nom::{
    bytes::complete::{tag, take},
    combinator::map_res,
    number::complete::{be_u16, be_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;
use std::path::Path;

#[derive(Debug)]
enum ModbusFunctionCode {
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
    transaction_id: u16,
    protocol_id: u16,
    length: u16,
    unit_id: u8,
    function_code: ModbusFunctionCode,
    data: Vec<u8>,
}

fn parse_modbus_function_code(input: &[u8]) -> IResult<&[u8], ModbusFunctionCode> {
    let (input, code) = be_u8(input)?;
    let function_code = match code {
        1 => ModbusFunctionCode::ReadCoils,
        2 => ModbusFunctionCode::ReadDiscreteInputs,
        3 => ModbusFunctionCode::ReadHoldingRegisters,
        4 => ModbusFunctionCode::ReadInputRegisters,
        5 => ModbusFunctionCode::WriteSingleCoil,
        6 => ModbusFunctionCode::WriteSingleRegister,
        15 => ModbusFunctionCode::WriteMultipleCoils,
        16 => ModbusFunctionCode::WriteMultipleRegisters,
        _ => ModbusFunctionCode::Other(code),
    };
    Ok((input, function_code))
}

fn parse_modbus_request(input: &[u8]) -> IResult<&[u8], ModbusRequest> {
    let (input, (transaction_id, protocol_id, length, unit_id, function_code)) =
        tuple((be_u16, be_u16, be_u16, be_u8, parse_modbus_function_code))(input)?;
    let data_length = length - 2; // Subtract unit_id and function_code bytes
    let (input, data) = take(data_length)(input)?;
    Ok((
        input,
        ModbusRequest {
            transaction_id,
            protocol_id,
            length,
            unit_id,
            function_code,
            data: data.to_vec(),
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary file>", args[0]);
        return;
    }

    let path = Path::new(&args[1]);
    let mut file = match File::open(&path) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Error opening file {}: {}", path.display(), err);
            return;
        }
    };

    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Ok(_) => (),
        Err(err) => {
            eprintln!("Error reading file {}: {}", path.display(), err);
            return;
        }
    }

    match parse_modbus_request(&buffer) {
        Ok((_, request)) => println!("{:?}", request),
        Err(err) => eprintln!("Failed to parse Modbus request: {:?}", err),
    }
}