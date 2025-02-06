use std::env;
use std::fs::File;
use std::io::Read;
use nom::{bytes::complete::take, combinator::map, number::complete::{be_u8, be_u16}, IResult, error::ErrorKind};
use nom::error::context;

const MIN_MODBUS_MESSAGE_LENGTH: usize = 4;

#[derive(Debug)]
enum FunctionCode {
    ReadCoilStatus,
    ReadInputStatus,
    ReadHoldingRegisters,
    ReadInputRegisters,
    ForceSingleCoil,
    PresetSingleRegister,
    ForceMultipleCoils,
    PresetMultipleRegisters,
    ReportSlaveId,
    ReadFileRecord,
    WriteFileRecord,
    MaskWriteRegister,
    ReadWriteMultipleRegisters,
    ReadFIFOQueue,
    ReadDeviceIdentification,
    ReadDeviceIdentificationExtended,
    Reserved(ExceptionCode),
}

impl FunctionCode {
    fn from_byte(byte: u8) -> Self {
        match byte {
            0x01 => FunctionCode::ReadCoilStatus,
            0x02 => FunctionCode::ReadInputStatus,
            0x03 => FunctionCode::ReadHoldingRegisters,
            0x04 => FunctionCode::ReadInputRegisters,
            0x05 => FunctionCode::ForceSingleCoil,
            0x06 => FunctionCode::PresetSingleRegister,
            0x0F => FunctionCode::ForceMultipleCoils,
            0x10 => FunctionCode::PresetMultipleRegisters,
            0x11 => FunctionCode::ReportSlaveId,
            0x14 => FunctionCode::ReadFileRecord,
            0x15 => FunctionCode::WriteFileRecord,
            0x16 => FunctionCode::MaskWriteRegister,
            0x17 => FunctionCode::ReadWriteMultipleRegisters,
            0x18 => FunctionCode::ReadFIFOQueue,
            0x2B => FunctionCode::ReadDeviceIdentification,
            0x2C => FunctionCode::ReadDeviceIdentificationExtended,
            0xE0..=0xE5 => FunctionCode::Reserved(ExceptionCode::from_byte(byte - 0xE0)),
            _ => panic!("Invalid function code"),
        }
    }
}

#[derive(Debug)]
enum ExceptionCode {
    IllegalFunction,
    IllegalDataAddress,
    IllegalDataValue,
    SlaveDeviceFailure,
    Acknowledge,
    SlaveDeviceBusy,
    GatewayPathUnavailable,
    GatewayTargetDeviceFailedToRespond,
}

impl ExceptionCode {
    fn from_byte(byte: u8) -> Self {
        match byte {
            0x01 => ExceptionCode::IllegalFunction,
            0x02 => ExceptionCode::IllegalDataAddress,
            0x03 => ExceptionCode::IllegalDataValue,
            0x04 => ExceptionCode::SlaveDeviceFailure,
            0x05 => ExceptionCode::Acknowledge,
            0x06 => ExceptionCode::SlaveDeviceBusy,
            0x0A => ExceptionCode::GatewayPathUnavailable,
            0x0B => ExceptionCode::GatewayTargetDeviceFailedToRespond,
            _ => panic!("Invalid exception code"),
        }
    }
}

#[derive(Debug)]
struct ModbusMessage {
    slave_id: u8,
    function_code: FunctionCode,
    data: Vec<u8>,
    crc: Option<u16>,
}

fn parse_modbus_message(input: &[u8]) -> IResult<&[u8], ModbusMessage> {
    let (input, slave_id) = be_u8(input)?;
    let (input, function_code) = map(be_u8, FunctionCode::from_byte)(input)?;
    let (input, data_length) = be_u8(input)?;
    let (input, data) = take(data_length as usize)(input)?;
    let (input, crc) = if input.len() >= 2 {
        let (input, crc) = be_u16(input)?;
        (input, Some(crc))
    } else {
        (input, None)
    };

    Ok((input, ModbusMessage { slave_id, function_code, data: data.to_vec(), crc }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    if buffer.len() < MIN_MODBUS_MESSAGE_LENGTH {
        println!("Invalid Modbus message length");
        return;
    }

    match parse_modbus_message(&buffer) {
        Ok((remaining, message)) => {
            println!("Slave ID: {}", message.slave_id);
            println!("Function Code: {:?}", message.function_code);
            println!("Data: {:?}", message.data);
            println!("CRC: {:?}", message.crc);
            println!("Remaining: {:?}", remaining);
        }
        Err(err) => {
            println!("Error parsing Modbus message: {:?}", err);
        }
    }
}