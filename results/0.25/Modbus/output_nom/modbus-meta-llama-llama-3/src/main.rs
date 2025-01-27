use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::take,
    number::complete::{be_u8, be_u16},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;
use std::path::Path;

#[derive(Debug, PartialEq)]
enum ModbusFunctionCode {
    ReadCoils = 0x01,
    ReadDiscreteInputs = 0x02,
    ReadHoldingRegisters = 0x03,
    ReadInputRegisters = 0x04,
    WriteSingleCoil = 0x05,
    WriteSingleHoldingRegister = 0x06,
    ReadExceptionStatus = 0x07,
    Diagnostics = 0x08,
    ProgramThatRelationFile = 0x14,
   Polling = 0x0E,
    ReportSlaveID = 0x11,
    WriteMaskRegister = 0x16,
}

#[derive(Debug, PartialEq)]
enum ModbusExceptionCode {
    IllegalFunction = 0x01,
    IllegalDataAddress = 0x02,
    IllegalDataValue = 0x03,
    SlaveDeviceFailure = 0x04,
    Acknowledge = 0x05,
    SlaveDeviceBusy = 0x06,
    NegativeAcknowledge = 0x07,
    MemoryParityError = 0x08,
    GatewayPathUnavailable = 0x0A,
    GatewayTargetDeviceFailedToRespond = 0x0B,
}

#[derive(Debug, PartialEq)]
struct ModbusRequest {
    transaction_id: u16,
    protocol_id: u16,
    length: u16,
    unit_id: u8,
    function_code: ModbusFunctionCode,
    data: Vec<u8>,
}

#[derive(Debug, PartialEq)]
struct ModbusResponse {
    transaction_id: u16,
    protocol_id: u16,
    length: u16,
    unit_id: u8,
    function_code: ModbusFunctionCode,
    data: Vec<u8>,
}

#[derive(Debug, PartialEq)]
struct ModbusExceptionResponse {
    transaction_id: u16,
    protocol_id: u16,
    length: u16,
    unit_id: u8,
    function_code: ModbusFunctionCode,
    exception_code: ModbusExceptionCode,
}

fn parse_modbus_request(input: &[u8]) -> IResult<&[u8], ModbusRequest> {
    let (input, transaction_id) = be_u16(input)?;
    let (input, protocol_id) = be_u16(input)?;
    let (input, length) = be_u16(input)?;
    let (input, unit_id) = be_u8(input)?;
    let (input, function_code_raw) = be_u8(input)?;
    let function_code = match function_code_raw {
        0x01 => ModbusFunctionCode::ReadCoils,
        0x02 => ModbusFunctionCode::ReadDiscreteInputs,
        0x03 => ModbusFunctionCode::ReadHoldingRegisters,
        0x04 => ModbusFunctionCode::ReadInputRegisters,
        0x05 => ModbusFunctionCode::WriteSingleCoil,
        0x06 => ModbusFunctionCode::WriteSingleHoldingRegister,
        0x07 => ModbusFunctionCode::ReadExceptionStatus,
        0x08 => ModbusFunctionCode::Diagnostics,
        0x14 => ModbusFunctionCode::ProgramThatRelationFile,
        0x0E => ModbusFunctionCode::Polling,
        0x11 => ModbusFunctionCode::ReportSlaveID,
        0x16 => ModbusFunctionCode::WriteMaskRegister,
        _ => panic!("Invalid function code"),
    };
    let (input, data) = take(length as usize - 2)(input)?;
    Ok((input, ModbusRequest {
        transaction_id,
        protocol_id,
        length,
        unit_id,
        function_code,
        data: data.to_vec(),
    }))
}

fn parse_modbus_response(input: &[u8]) -> IResult<&[u8], ModbusResponse> {
    let (input, transaction_id) = be_u16(input)?;
    let (input, protocol_id) = be_u16(input)?;
    let (input, length) = be_u16(input)?;
    let (input, unit_id) = be_u8(input)?;
    let (input, function_code_raw) = be_u8(input)?;
    let function_code = match function_code_raw {
        0x01 => ModbusFunctionCode::ReadCoils,
        0x02 => ModbusFunctionCode::ReadDiscreteInputs,
        0x03 => ModbusFunctionCode::ReadHoldingRegisters,
        0x04 => ModbusFunctionCode::ReadInputRegisters,
        0x05 => ModbusFunctionCode::WriteSingleCoil,
        0x06 => ModbusFunctionCode::WriteSingleHoldingRegister,
        0x07 => ModbusFunctionCode::ReadExceptionStatus,
        0x08 => ModbusFunctionCode::Diagnostics,
        0x14 => ModbusFunctionCode::ProgramThatRelationFile,
        0x0E => ModbusFunctionCode::Polling,
        0x11 => ModbusFunctionCode::ReportSlaveID,
        0x16 => ModbusFunctionCode::WriteMaskRegister,
        _ => panic!("Invalid function code"),
    };
    let (input, data) = take(length as usize - 2)(input)?;
    Ok((input, ModbusResponse {
        transaction_id,
        protocol_id,
        length,
        unit_id,
        function_code,
        data: data.to_vec(),
    }))
}

fn parse_modbus_exception_response(input: &[u8]) -> IResult<&[u8], ModbusExceptionResponse> {
    let (input, transaction_id) = be_u16(input)?;
    let (input, protocol_id) = be_u16(input)?;
    let (input, length) = be_u16(input)?;
    let (input, unit_id) = be_u8(input)?;
    let (input, function_code_raw) = be_u8(input)?;
    let function_code = match function_code_raw {
        0x01 => ModbusFunctionCode::ReadCoils,
        0x02 => ModbusFunctionCode::ReadDiscreteInputs,
        0x03 => ModbusFunctionCode::ReadHoldingRegisters,
        0x04 => ModbusFunctionCode::ReadInputRegisters,
        0x05 => ModbusFunctionCode::WriteSingleCoil,
        0x06 => ModbusFunctionCode::WriteSingleHoldingRegister,
        0x07 => ModbusFunctionCode::ReadExceptionStatus,
        0x08 => ModbusFunctionCode::Diagnostics,
        0x14 => ModbusFunctionCode::ProgramThatRelationFile,
        0x0E => ModbusFunctionCode::Polling,
        0x11 => ModbusFunctionCode::ReportSlaveID,
        0x16 => ModbusFunctionCode::WriteMaskRegister,
        _ => panic!("Invalid function code"),
    };
    let (input, exception_code) = map(be_u8, |x| match x {
        0x01 => ModbusExceptionCode::IllegalFunction,
        0x02 => ModbusExceptionCode::IllegalDataAddress,
        0x03 => ModbusExceptionCode::IllegalDataValue,
        0x04 => ModbusExceptionCode::SlaveDeviceFailure,
        0x05 => ModbusExceptionCode::Acknowledge,
        0x06 => ModbusExceptionCode::SlaveDeviceBusy,
        0x07 => ModbusExceptionCode::NegativeAcknowledge,
        0x08 => ModbusExceptionCode::MemoryParityError,
        0x0A => ModbusExceptionCode::GatewayPathUnavailable,
        0x0B => ModbusExceptionCode::GatewayTargetDeviceFailedToRespond,
        _ => panic!("Invalid exception code"),
    })(input)?;
    Ok((input, ModbusExceptionResponse {
        transaction_id,
        protocol_id,
        length,
        unit_id,
        function_code,
        exception_code,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <input_file>", args[0]);
    }
    let input_file = &args[1];
    let path = Path::new(input_file);
    let mut file = File::open(path).expect("Failed to open file");
    let mut input = Vec::new();
    file.read_to_end(&mut input).expect("Failed to read file");
    match parse_modbus_request(&input) {
        Ok((_, request)) => println!("{:?}", request),
        Err(_) => match parse_modbus_response(&input) {
            Ok((_, response)) => println!("{:?}", response),
            Err(_) => match parse_modbus_exception_response(&input) {
                Ok((_, exception_response)) => println!("{:?}", exception_response),
                Err(e) => panic!("Error parsing input: {}", e),
            },
        },
    }
}