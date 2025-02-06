use nom::{
    bytes::complete::{take, take_while},
    combinator::{map},
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{Read};

#[derive(Debug, PartialEq, Clone)]
enum ModbusFunctionCode {
    ReadCoilStatus = 0x01,
    ReadInputStatus = 0x02,
    ReadHoldingRegisters = 0x03,
    ReadInputRegisters = 0x04,
    WriteSingleCoil = 0x05,
    WriteSingleHoldingRegister = 0x06,
    WriteMultipleCoils = 0x0F,
    WriteMultipleHoldingRegisters = 0x10,
    ReportSlaveID = 0x11,
    ReadFileRecord = 0x14,
    WriteFileRecord = 0x15,
    MaskWriteRegister = 0x16,
    ReadWriteMultipleRegisters = 0x17,
    ReadFIFOQueue = 0x18,
    EncapsulatedInterfaceTransport = 0x2B,
}

#[derive(Debug, PartialEq)]
enum ModbusErrorCode {
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
enum ModbusReferenceType {
    Register = 0x06,
    Coil = 0x07,
}

#[derive(Debug, PartialEq)]
struct ModbusMessage {
    address: u8,
    function_code: ModbusFunctionCode,
    data: Vec<u8>,
    crc: u16,
}

fn parse_modbus_address(input: &[u8]) -> IResult<&[u8], u8> {
    map(take(1u8), |x: &[u8]| x[0])(input)
}

fn parse_modbus_function_code(input: &[u8]) -> IResult<&[u8], ModbusFunctionCode> {
    map(
        be_u8,
        |function_code: u8| match function_code {
            0x01 => ModbusFunctionCode::ReadCoilStatus,
            0x02 => ModbusFunctionCode::ReadInputStatus,
            0x03 => ModbusFunctionCode::ReadHoldingRegisters,
            0x04 => ModbusFunctionCode::ReadInputRegisters,
            0x05 => ModbusFunctionCode::WriteSingleCoil,
            0x06 => ModbusFunctionCode::WriteSingleHoldingRegister,
            0x0F => ModbusFunctionCode::WriteMultipleCoils,
            0x10 => ModbusFunctionCode::WriteMultipleHoldingRegisters,
            0x11 => ModbusFunctionCode::ReportSlaveID,
            0x14 => ModbusFunctionCode::ReadFileRecord,
            0x15 => ModbusFunctionCode::WriteFileRecord,
            0x16 => ModbusFunctionCode::MaskWriteRegister,
            0x17 => ModbusFunctionCode::ReadWriteMultipleRegisters,
            0x18 => ModbusFunctionCode::ReadFIFOQueue,
            0x2B => ModbusFunctionCode::EncapsulatedInterfaceTransport,
            _ => panic!("Invalid function code"),
        },
    )(input)
}

fn parse_modbus_data<'a>(input: &'a [u8], function_code: &'a ModbusFunctionCode) -> IResult<&'a [u8], Vec<u8>> {
    match function_code {
        ModbusFunctionCode::ReadCoilStatus => {
            let (input, _) = take(1u8)(input)?;
            let (input, data) = take_while(|x| x != 0)(input)?;
            Ok((input, data.to_vec()))
        }
        ModbusFunctionCode::ReadInputStatus => {
            let (input, _) = take(1u8)(input)?;
            let (input, data) = take_while(|x| x != 0)(input)?;
            Ok((input, data.to_vec()))
        }
        ModbusFunctionCode::ReadHoldingRegisters => {
            let (input, _) = take(2u8)(input)?;
            let (input, _) = take(2u8)(input)?;
            let (input, data) = take_while(|x| x != 0)(input)?;
            Ok((input, data.to_vec()))
        }
        ModbusFunctionCode::ReadInputRegisters => {
            let (input, _) = take(2u8)(input)?;
            let (input, _) = take(2u8)(input)?;
            let (input, data) = take_while(|x| x != 0)(input)?;
            Ok((input, data.to_vec()))
        }
        ModbusFunctionCode::WriteSingleCoil => {
            let (input, _) = take(2u8)(input)?;
            let (input, data) = take(1u8)(input)?;
            Ok((input, data.to_vec()))
        }
        ModbusFunctionCode::WriteSingleHoldingRegister => {
            let (input, _) = take(2u8)(input)?;
            let (input, data) = take(2u8)(input)?;
            Ok((input, data.to_vec()))
        }
        ModbusFunctionCode::WriteMultipleCoils => {
            let (input, _) = take(2u8)(input)?;
            let (input, _) = take(2u8)(input)?;
            let (input, data) = take_while(|x| x != 0)(input)?;
            Ok((input, data.to_vec()))
        }
        ModbusFunctionCode::WriteMultipleHoldingRegisters => {
            let (input, _) = take(2u8)(input)?;
            let (input, _) = take(2u8)(input)?;
            let (input, data) = take_while(|x| x != 0)(input)?;
            Ok((input, data.to_vec()))
        }
        ModbusFunctionCode::ReportSlaveID => {
            let (input, data) = take_while(|x| x != 0)(input)?;
            Ok((input, data.to_vec()))
        }
        ModbusFunctionCode::ReadFileRecord => {
            let (input, _) = take(2u8)(input)?;
            let (input, _) = take(2u8)(input)?;
            let (input, data) = take_while(|x| x != 0)(input)?;
            Ok((input, data.to_vec()))
        }
        ModbusFunctionCode::WriteFileRecord => {
            let (input, _) = take(2u8)(input)?;
            let (input, _) = take(2u8)(input)?;
            let (input, data) = take_while(|x| x != 0)(input)?;
            Ok((input, data.to_vec()))
        }
        ModbusFunctionCode::MaskWriteRegister => {
            let (input, _) = take(2u8)(input)?;
            let (input, _) = take(2u8)(input)?;
            let (input, data) = take(2u8)(input)?;
            Ok((input, data.to_vec()))
        }
        ModbusFunctionCode::ReadWriteMultipleRegisters => {
            let (input, _) = take(2u8)(input)?;
            let (input, _) = take(2u8)(input)?;
            let (input, data) = take_while(|x| x != 0)(input)?;
            Ok((input, data.to_vec()))
        }
        ModbusFunctionCode::ReadFIFOQueue => {
            let (input, _) = take(2u8)(input)?;
            let (input, data) = take_while(|x| x != 0)(input)?;
            Ok((input, data.to_vec()))
        }
        ModbusFunctionCode::EncapsulatedInterfaceTransport => {
            let (input, _) = take(1u8)(input)?;
            let (input, _) = take(1u8)(input)?;
            let (input, data) = take_while(|x| x != 0)(input)?;
            Ok((input, data.to_vec()))
        }
    }
}

fn parse_modbus_crc(input: &[u8]) -> IResult<&[u8], u16> {
    be_u16(input)
}

fn parse_modbus_message(input: &[u8]) -> IResult<&[u8], ModbusMessage> {
    let (input, address) = parse_modbus_address(input)?;
    let (input, function_code) = parse_modbus_function_code(input)?;
    let (input, data) = parse_modbus_data(input, &function_code.clone())?;
    let (input, crc) = parse_modbus_crc(input)?;
    Ok((
        input,
        ModbusMessage {
            address,
            function_code: function_code.clone(),
            data,
            crc,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <input_file>", args[0]);
    }
    let input_file = &args[1];
    let mut file = match File::open(input_file) {
        Ok(file) => file,
        Err(_) => panic!("Failed to open file {}", input_file),
    };
    let mut input = Vec::new();
    match file.read_to_end(&mut input) {
        Ok(_) => (),
        Err(_) => panic!("Failed to read file {}", input_file),
    }
    match parse_modbus_message(&input) {
        Ok((_, message)) => println!("{:?}", message),
        Err(err) => panic!("Failed to parse message: {:?}", err),
    }
}