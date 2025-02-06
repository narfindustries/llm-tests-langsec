use nom::{
    bytes::complete::{take, take_while_m_n},
    combinator::{map, opt},
    error::{context, ErrorKind, ParseError},
    multi::{length_data},
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
    clone::Clone,
    fmt::Debug,
};

#[derive(Debug, Clone, PartialEq)]
enum FunctionCode {
    ReadCoilStatus = 0x01,
    ReadInputStatus = 0x02,
    ReadHoldingRegisters = 0x03,
    ReadInputRegisters = 0x04,
    WriteSingleCoil = 0x05,
    WriteSingleRegister = 0x06,
    ReadExceptionStatus = 0x07,
    WriteMultipleCoils = 0x0F,
    WriteMultipleRegisters = 0x10,
    ReportSlaveID = 0x11,
    ProgramController = 0x12,
    Program = 0x13,
    ProgramExtended = 0x14,
    WriteMultipleCoilsExtended = 0x15,
    WriteMultipleRegistersExtended = 0x16,
    ReportSlaveIDExtended = 0x17,
}

impl From<u8> for FunctionCode {
    fn from(value: u8) -> Self {
        match value {
            0x01 => FunctionCode::ReadCoilStatus,
            0x02 => FunctionCode::ReadInputStatus,
            0x03 => FunctionCode::ReadHoldingRegisters,
            0x04 => FunctionCode::ReadInputRegisters,
            0x05 => FunctionCode::WriteSingleCoil,
            0x06 => FunctionCode::WriteSingleRegister,
            0x07 => FunctionCode::ReadExceptionStatus,
            0x0F => FunctionCode::WriteMultipleCoils,
            0x10 => FunctionCode::WriteMultipleRegisters,
            0x11 => FunctionCode::ReportSlaveID,
            0x12 => FunctionCode::ProgramController,
            0x13 => FunctionCode::Program,
            0x14 => FunctionCode::ProgramExtended,
            0x15 => FunctionCode::WriteMultipleCoilsExtended,
            0x16 => FunctionCode::WriteMultipleRegistersExtended,
            0x17 => FunctionCode::ReportSlaveIDExtended,
            _ => panic!("Invalid function code"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum ErrorCode {
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

impl From<u8> for ErrorCode {
    fn from(value: u8) -> Self {
        match value {
            0x01 => ErrorCode::IllegalFunction,
            0x02 => ErrorCode::IllegalDataAddress,
            0x03 => ErrorCode::IllegalDataValue,
            0x04 => ErrorCode::SlaveDeviceFailure,
            0x05 => ErrorCode::Acknowledge,
            0x06 => ErrorCode::SlaveDeviceBusy,
            0x07 => ErrorCode::NegativeAcknowledge,
            0x08 => ErrorCode::MemoryParityError,
            0x0A => ErrorCode::GatewayPathUnavailable,
            0x0B => ErrorCode::GatewayTargetDeviceFailedToRespond,
            _ => panic!("Invalid error code"),
        }
    }
}

fn parse_address(input: &[u8]) -> IResult<&[u8], u8> {
    context("address", map(take(1u8), |value: &[u8]| value[0]))(input)
}

fn parse_function_code(input: &[u8]) -> IResult<&[u8], FunctionCode> {
    context(
        "function code",
        map(take(1u8), |value: &[u8]| FunctionCode::from(value[0])),
    )(input)
}

fn parse_byte_count(input: &[u8]) -> IResult<&[u8], u8> {
    context("byte count", map(take(1u8), |value: &[u8]| value[0]))(input)
}

fn parse_reference_number(input: &[u8]) -> IResult<&[u8], u16> {
    context("reference number", be_u16)(input)
}

fn parse_register_address(input: &[u8]) -> IResult<&[u8], u16> {
    context("register address", be_u16)(input)
}

fn parse_register_count(input: &[u8]) -> IResult<&[u8], u16> {
    context("register count", be_u16)(input)
}

fn parse_data(input: &[u8], function_code: FunctionCode) -> IResult<&[u8], Vec<u8>> {
    match function_code {
        FunctionCode::ReadCoilStatus => {
            context("data", map(length_data(be_u8), |value: &[u8]| value.to_vec()))(input)
        }
        FunctionCode::ReadInputStatus => {
            context("data", map(length_data(be_u8), |value: &[u8]| value.to_vec()))(input)
        }
        FunctionCode::ReadHoldingRegisters => {
            context("data", map(length_data(be_u16), |value: &[u8]| value.to_vec()))(input)
        }
        FunctionCode::ReadInputRegisters => {
            context("data", map(length_data(be_u16), |value: &[u8]| value.to_vec()))(input)
        }
        FunctionCode::WriteSingleCoil => {
            context("data", map(take(2u8), |value: &[u8]| value.to_vec()))(input)
        }
        FunctionCode::WriteSingleRegister => {
            context("data", map(take(2u8), |value: &[u8]| value.to_vec()))(input)
        }
        FunctionCode::ReadExceptionStatus => {
            context("data", map(take(1u8), |value: &[u8]| value.to_vec()))(input)
        }
        FunctionCode::WriteMultipleCoils => {
            context("data", map(length_data(be_u8), |value: &[u8]| value.to_vec()))(input)
        }
        FunctionCode::WriteMultipleRegisters => {
            context("data", map(length_data(be_u16), |value: &[u8]| value.to_vec()))(input)
        }
        FunctionCode::ReportSlaveID => {
            context("data", map(take_while_m_n(2, 255, |c| c != 0), |value: &[u8]| value.to_vec()))(input)
        }
        _ => Ok((input, Vec::new())),
    }
}

fn parse_error_code(input: &[u8]) -> IResult<&[u8], ErrorCode> {
    context(
        "error code",
        map(take(1u8), |value: &[u8]| ErrorCode::from(value[0])),
    )(input)
}

fn parse_crc(input: &[u8]) -> IResult<&[u8], u16> {
    context("crc", be_u16)(input)
}

fn parse_modbus(input: &[u8]) -> IResult<&[u8], (u8, FunctionCode, Vec<u8>)> {
    let (input, address) = parse_address(input)?;
    let (input, function_code) = parse_function_code(input)?;
    let (input, _) = opt(parse_byte_count)(input)?;
    let (input, _) = opt(parse_reference_number)(input)?;
    let (input, _) = opt(parse_register_address)(input)?;
    let (input, _) = opt(parse_register_count)(input)?;
    let (input, data) = parse_data(input, function_code.clone())?;
    let (input, _) = opt(parse_error_code)(input)?;
    let (input, _) = opt(parse_crc)(input)?;
    Ok((input, (address, function_code, data)))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <input_file>", args[0]);
    }

    let file = File::open(&args[1]).expect("Failed to open file");
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input).expect("Failed to read file");

    let result = parse_modbus(&input);
    match result {
        Ok((remaining, (address, function_code, data))) => {
            println!("Address: {}", address);
            println!("Function Code: {:?}", function_code);
            println!("Data: {:?}", data);
            println!("Remaining: {:?}", remaining);
        }
        Err(err) => {
            println!("Error: {:?}", err);
        }
    }
}