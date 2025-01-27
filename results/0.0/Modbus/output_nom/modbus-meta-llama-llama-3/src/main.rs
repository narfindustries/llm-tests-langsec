use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::take_while_m_n,
    number::complete::{be_u8, be_u16},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
};

#[derive(Debug, PartialEq)]
enum ModbusFunctionCode {
    ReadCoils = 0x01,
    ReadDiscreteInputs = 0x02,
    ReadHoldingRegisters = 0x03,
    ReadInputRegisters = 0x04,
    WriteSingleCoil = 0x05,
    WriteSingleRegister = 0x06,
    ReadExceptionStatus = 0x07,
    Diagnostics = 0x08,
    GetCommEventCounter = 0x0B,
    GetCommEventLog = 0x0C,
    WriteMultipleCoils = 0x0F,
    WriteMultipleRegisters = 0x10,
    ReportSlaveId = 0x11,
    ReadFileRecord = 0x14,
    WriteFileRecord = 0x15,
    MaskWriteRegister = 0x16,
    ReadWriteMultipleRegisters = 0x17,
    ReadFifoQueue = 0x18,
    EncapsulatedInterfaceTransport = 0x2B,
}

#[derive(Debug, PartialEq)]
enum ModbusExceptionCode {
    IllegalFunction = 0x01,
    IllegalDataAddress = 0x02,
    IllegalDataValue = 0x03,
    SlaveDeviceFailure = 0x04,
    Acknowledge = 0x05,
    SlaveDeviceBusy = 0x06,
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
    function_code: u8,
    exception_code: ModbusExceptionCode,
}

fn parse_modbus_request(input: &[u8]) -> IResult<&[u8], ModbusRequest> {
    let (input, transaction_id) = be_u16(input)?;
    let (input, protocol_id) = be_u16(input)?;
    let (input, length) = be_u16(input)?;
    let (input, unit_id) = be_u8(input)?;
    let (input, function_code) = map(be_u8, |x| match x {
        0x01 => ModbusFunctionCode::ReadCoils,
        0x02 => ModbusFunctionCode::ReadDiscreteInputs,
        0x03 => ModbusFunctionCode::ReadHoldingRegisters,
        0x04 => ModbusFunctionCode::ReadInputRegisters,
        0x05 => ModbusFunctionCode::WriteSingleCoil,
        0x06 => ModbusFunctionCode::WriteSingleRegister,
        0x07 => ModbusFunctionCode::ReadExceptionStatus,
        0x08 => ModbusFunctionCode::Diagnostics,
        0x0B => ModbusFunctionCode::GetCommEventCounter,
        0x0C => ModbusFunctionCode::GetCommEventLog,
        0x0F => ModbusFunctionCode::WriteMultipleCoils,
        0x10 => ModbusFunctionCode::WriteMultipleRegisters,
        0x11 => ModbusFunctionCode::ReportSlaveId,
        0x14 => ModbusFunctionCode::ReadFileRecord,
        0x15 => ModbusFunctionCode::WriteFileRecord,
        0x16 => ModbusFunctionCode::MaskWriteRegister,
        0x17 => ModbusFunctionCode::ReadWriteMultipleRegisters,
        0x18 => ModbusFunctionCode::ReadFifoQueue,
        0x2B => ModbusFunctionCode::EncapsulatedInterfaceTransport,
        _ => panic!("Invalid function code"),
    })(input)?;
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
    let (input, function_code) = map(be_u8, |x| match x {
        0x01 => ModbusFunctionCode::ReadCoils,
        0x02 => ModbusFunctionCode::ReadDiscreteInputs,
        0x03 => ModbusFunctionCode::ReadHoldingRegisters,
        0x04 => ModbusFunctionCode::ReadInputRegisters,
        0x05 => ModbusFunctionCode::WriteSingleCoil,
        0x06 => ModbusFunctionCode::WriteSingleRegister,
        0x07 => ModbusFunctionCode::ReadExceptionStatus,
        0x08 => ModbusFunctionCode::Diagnostics,
        0x0B => ModbusFunctionCode::GetCommEventCounter,
        0x0C => ModbusFunctionCode::GetCommEventLog,
        0x0F => ModbusFunctionCode::WriteMultipleCoils,
        0x10 => ModbusFunctionCode::WriteMultipleRegisters,
        0x11 => ModbusFunctionCode::ReportSlaveId,
        0x14 => ModbusFunctionCode::ReadFileRecord,
        0x15 => ModbusFunctionCode::WriteFileRecord,
        0x16 => ModbusFunctionCode::MaskWriteRegister,
        0x17 => ModbusFunctionCode::ReadWriteMultipleRegisters,
        0x18 => ModbusFunctionCode::ReadFifoQueue,
        0x2B => ModbusFunctionCode::EncapsulatedInterfaceTransport,
        _ => panic!("Invalid function code"),
    })(input)?;
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
    let (input, function_code) = be_u8(input)?;
    let (input, exception_code) = map(be_u8, |x| match x {
        0x01 => ModbusExceptionCode::IllegalFunction,
        0x02 => ModbusExceptionCode::IllegalDataAddress,
        0x03 => ModbusExceptionCode::IllegalDataValue,
        0x04 => ModbusExceptionCode::SlaveDeviceFailure,
        0x05 => ModbusExceptionCode::Acknowledge,
        0x06 => ModbusExceptionCode::SlaveDeviceBusy,
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
    let file = File::open(&args[1]).unwrap();
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input).unwrap();
    match parse_modbus_request(&input) {
        Ok((_, request)) => println!("{:?}", request),
        Err(_) => match parse_modbus_response(&input) {
            Ok((_, response)) => println!("{:?}", response),
            Err(_) => match parse_modbus_exception_response(&input) {
                Ok((_, exception_response)) => println!("{:?}", exception_response),
                Err(e) => panic!("Error parsing input: {:?}", e),
            },
        },
    }
}