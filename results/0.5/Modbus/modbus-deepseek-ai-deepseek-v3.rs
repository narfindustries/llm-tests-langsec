use nom::{
    bytes::complete::take,
    number::complete::{be_u16, be_u8},
    sequence::tuple,
    IResult,
    error::{Error, ErrorKind},
};
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug, Clone)]
struct ModbusHeader {
    transaction_id: u16,
    protocol_id: u16,
    length: u16,
    unit_id: u8,
}

#[derive(Debug, Clone)]
enum ModbusFunction {
    ReadCoils,
    ReadDiscreteInputs,
    ReadHoldingRegisters,
    ReadInputRegisters,
    WriteSingleCoil,
    WriteSingleRegister,
    ReadExceptionStatus,
    Diagnostics,
    GetCommEventCounter,
    GetCommEventLog,
    WriteMultipleCoils,
    WriteMultipleRegisters,
    ReportServerID,
    ReadFileRecord,
    WriteFileRecord,
    MaskWriteRegister,
    ReadWriteMultipleRegisters,
    ReadFIFOQueue,
    EncapsulatedInterfaceTransport,
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
    error_code: Option<u8>,
    exception_code: Option<u8>,
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
        0x07 => ModbusFunction::ReadExceptionStatus,
        0x08 => ModbusFunction::Diagnostics,
        0x0B => ModbusFunction::GetCommEventCounter,
        0x0C => ModbusFunction::GetCommEventLog,
        0x0F => ModbusFunction::WriteMultipleCoils,
        0x10 => ModbusFunction::WriteMultipleRegisters,
        0x11 => ModbusFunction::ReportServerID,
        0x14 => ModbusFunction::ReadFileRecord,
        0x15 => ModbusFunction::WriteFileRecord,
        0x16 => ModbusFunction::MaskWriteRegister,
        0x17 => ModbusFunction::ReadWriteMultipleRegisters,
        0x18 => ModbusFunction::ReadFIFOQueue,
        0x2B => ModbusFunction::EncapsulatedInterfaceTransport,
        _ => return Err(nom::Err::Error(Error::new(input, ErrorKind::Tag))),
    };
    Ok((input, function))
}

fn parse_modbus_request(input: &[u8]) -> IResult<&[u8], ModbusRequest> {
    let (input, header) = parse_modbus_header(input)?;
    let (input, function) = parse_modbus_function(input)?;
    let data_length = header.length as usize - 1; // Subtract function code
    let (input, data) = take(data_length)(input)?;
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
    let (input, error_code) = if (function.clone() as u8) & 0x80 != 0 {
        let (input, error_code) = be_u8(input)?;
        (input, Some(error_code))
    } else {
        (input, None)
    };
    let (input, exception_code) = if error_code.is_some() {
        let (input, exception_code) = be_u8(input)?;
        (input, Some(exception_code))
    } else {
        (input, None)
    };
    let data_length = header.length as usize - 1 - if error_code.is_some() { 2 } else { 0 };
    let (input, data) = take(data_length)(input)?;
    Ok((
        input,
        ModbusResponse {
            header,
            function,
            data: data.to_vec(),
            error_code,
            exception_code,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_modbus_request(&buffer) {
        Ok((_, request)) => println!("Parsed Request: {:?}", request),
        Err(_) => match parse_modbus_response(&buffer) {
            Ok((_, response)) => println!("Parsed Response: {:?}", response),
            Err(_) => println!("Failed to parse Modbus message"),
        },
    }
}