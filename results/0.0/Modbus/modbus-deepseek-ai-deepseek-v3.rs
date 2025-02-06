use nom::{
    bytes::complete::take,
    number::complete::{be_u16, be_u8},
    sequence::tuple,
    IResult, error::{Error, ErrorKind},
};
use std::env;
use std::fs::File;
use std::io::Read;

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
    Error(u8),
}

#[derive(Debug)]
struct ModbusMessage {
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
        code if code >= 0x80 => ModbusFunction::Error(code),
        _ => return Err(nom::Err::Error(Error::new(input, ErrorKind::Tag))),
    };
    Ok((input, function))
}

fn parse_modbus_message(input: &[u8]) -> IResult<&[u8], ModbusMessage> {
    let (input, header) = parse_modbus_header(input)?;
    let (input, function) = parse_modbus_function(input)?;
    let (input, data) = take(header.length - 1)(input)?;
    let (error_code, exception_code) = match function {
        ModbusFunction::Error(_) => {
            let (input, error_code) = be_u8(input)?;
            let (input, exception_code) = be_u8(input)?;
            (Some(error_code), Some(exception_code))
        }
        _ => (None, None),
    };
    Ok((
        input,
        ModbusMessage {
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
        eprintln!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_modbus_message(&buffer) {
        Ok((_, message)) => println!("{:?}", message),
        Err(e) => eprintln!("Failed to parse Modbus message: {:?}", e),
    }
}