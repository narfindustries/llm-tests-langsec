use nom::{
    be::*,
    bytes::complete::*,
    combinator::*,
    error::*,
    multi::*,
    number::complete::*,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug, PartialEq)]
enum ModbusFunctionCode {
    ReadCoils = 1,
    ReadDiscreteInputs = 2,
    ReadHoldingRegisters = 3,
    ReadInputRegisters = 4,
    WriteSingleCoil = 5,
    WriteSingleRegister = 6,
    WriteMultipleCoils = 15,
    WriteMultipleRegisters = 16,
    ReadExceptionStatus = 125, //Example of an exception
    // Add other function codes as needed...
}

#[derive(Debug, PartialEq)]
struct ModbusRequest {
    transaction_identifier: u16,
    protocol_identifier: u16,
    length: u16,
    unit_identifier: u8,
    function_code: ModbusFunctionCode,
    data: Vec<u8>,
}

#[derive(Debug, PartialEq)]
struct ModbusResponse {
    transaction_identifier: u16,
    protocol_identifier: u16,
    length: u16,
    unit_identifier: u8,
    function_code: ModbusFunctionCode,
    data: Vec<u8>,
}

fn modbus_header(input: &[u8]) -> nom::IResult<&[u8], (u16, u16, u16, u8)> {
    let (input, transaction_identifier) = be_u16(input)?;
    let (input, protocol_identifier) = be_u16(input)?;
    let (input, length) = be_u16(input)?;
    let (input, unit_identifier) = u8(input)?;
    Ok((input, (transaction_identifier, protocol_identifier, length, unit_identifier)))
}

fn modbus_request(input: &[u8]) -> nom::IResult<&[u8], ModbusRequest> {
    let (input, (transaction_identifier, protocol_identifier, length, unit_identifier)) =
        modbus_header(input)?;
    let (input, function_code_u8) = u8(input)?;
    let function_code = match ModbusFunctionCode::from(function_code_u8) {
        Some(fc) => fc,
        None => return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Custom(1)))), //Handle Unknown Function Codes
    };
    let (input, data) = take(length as usize)(input)?;

    Ok((
        input,
        ModbusRequest {
            transaction_identifier,
            protocol_identifier,
            length,
            unit_identifier,
            function_code,
            data: data.to_vec(),
        },
    ))
}


fn modbus_response(input: &[u8]) -> nom::IResult<&[u8], ModbusResponse> {
    let (input, (transaction_identifier, protocol_identifier, length, unit_identifier)) =
        modbus_header(input)?;
    let (input, function_code_u8) = u8(input)?;
    let function_code = match ModbusFunctionCode::from(function_code_u8) {
        Some(fc) => fc,
        None => return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Custom(1)))), //Handle Unknown Function Codes
    };
    let (input, data) = take(length as usize -1)(input)?; //Subtract 1 for function code

    Ok((
        input,
        ModbusResponse {
            transaction_identifier,
            protocol_identifier,
            length,
            unit_identifier,
            function_code,
            data: data.to_vec(),
        },
    ))
}


impl From<u8> for ModbusFunctionCode {
    fn from(code: u8) -> Option<Self> {
        match code {
            1 => Some(ModbusFunctionCode::ReadCoils),
            2 => Some(ModbusFunctionCode::ReadDiscreteInputs),
            3 => Some(ModbusFunctionCode::ReadHoldingRegisters),
            4 => Some(ModbusFunctionCode::ReadInputRegisters),
            5 => Some(ModbusFunctionCode::WriteSingleCoil),
            6 => Some(ModbusFunctionCode::WriteSingleRegister),
            15 => Some(ModbusFunctionCode::WriteMultipleCoils),
            16 => Some(ModbusFunctionCode::WriteMultipleRegisters),
            125 => Some(ModbusFunctionCode::ReadExceptionStatus),
            _ => None,
        }
    }
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match modbus_request(&buffer) {
        Ok((_, request)) => println!("Modbus Request: {:?}", request),
        Err(e) => println!("Error parsing Modbus Request: {:?}", e),
    }

    match modbus_response(&buffer) {
        Ok((_, response)) => println!("Modbus Response: {:?}", response),
        Err(e) => println!("Error parsing Modbus Response: {:?}", e),
    }
}
