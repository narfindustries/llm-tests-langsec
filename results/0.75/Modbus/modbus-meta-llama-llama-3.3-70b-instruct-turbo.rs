use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::length_data,
    number::complete::{be_u8, be_u16},
    sequence::{tuple, preceded},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
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
    ReportServerId = 0x11,
    ReadFileRecord = 0x14,
    WriteFileRecord = 0x15,
    MaskWriteRegister = 0x16,
    ReadWriteMultipleRegisters = 0x17,
    ReadUUID = 0x1B,
    Unknown(u8),
}

impl ModbusFunctionCode {
    fn from_code(code: u8) -> ModbusFunctionCode {
        match code {
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
            0x11 => ModbusFunctionCode::ReportServerId,
            0x14 => ModbusFunctionCode::ReadFileRecord,
            0x15 => ModbusFunctionCode::WriteFileRecord,
            0x16 => ModbusFunctionCode::MaskWriteRegister,
            0x17 => ModbusFunctionCode::ReadWriteMultipleRegisters,
            0x1B => ModbusFunctionCode::ReadUUID,
            _ => ModbusFunctionCode::Unknown(code),
        }
    }
}

#[derive(Debug)]
enum ModbusMessageData {
    ReadCoils {
        starting_address: u16,
        quantity: u16,
    },
    ReadDiscreteInputs {
        starting_address: u16,
        quantity: u16,
    },
    ReadHoldingRegisters {
        starting_address: u16,
        quantity: u16,
    },
    ReadInputRegisters {
        starting_address: u16,
        quantity: u16,
    },
    WriteSingleCoil {
        output_address: u16,
        output_value: u16,
    },
    WriteSingleRegister {
        register_address: u16,
        register_value: u16,
    },
    ReadExceptionStatus,
    Diagnostics {
        sub_function: u8,
    },
    GetCommEventCounter,
    GetCommEventLog {
        status: u8,
        event_count: u16,
    },
    WriteMultipleCoils {
        starting_address: u16,
        quantity: u16,
        output_values: Vec<u8>,
    },
    WriteMultipleRegisters {
        starting_address: u16,
        quantity: u16,
        register_values: Vec<u16>,
    },
    ReportServerId {
        status: u8,
        general_reference: u8,
    },
    ReadFileRecord {
        file_number: u16,
        record_number: u16,
        record_length: u16,
    },
    WriteFileRecord {
        file_number: u16,
        record_number: u16,
        record_data: Vec<u8>,
    },
    MaskWriteRegister {
        register_address: u16,
        and_mask: u16,
        or_mask: u16,
    },
    ReadWriteMultipleRegisters {
        read_starting_address: u16,
        read_quantity: u16,
        write_starting_address: u16,
        write_quantity: u16,
        write_register_values: Vec<u16>,
    },
    ReadUUID,
    Unknown(Vec<u8>),
}

fn parse_modbus_message(input: &[u8]) -> IResult<&[u8], (u8, ModbusMessageData)> {
    let (input, slave_address) = be_u8(input)?;
    let (input, function_code) = be_u8(input)?;
    let function_code = ModbusFunctionCode::from_code(function_code);
    let (input, data) = match function_code {
        ModbusFunctionCode::ReadCoils => {
            let (input, starting_address) = be_u16(input)?;
            let (input, quantity) = be_u16(input)?;
            Ok((input, ModbusMessageData::ReadCoils { starting_address, quantity }))
        }
        ModbusFunctionCode::ReadDiscreteInputs => {
            let (input, starting_address) = be_u16(input)?;
            let (input, quantity) = be_u16(input)?;
            Ok((input, ModbusMessageData::ReadDiscreteInputs { starting_address, quantity }))
        }
        ModbusFunctionCode::ReadHoldingRegisters => {
            let (input, starting_address) = be_u16(input)?;
            let (input, quantity) = be_u16(input)?;
            Ok((input, ModbusMessageData::ReadHoldingRegisters { starting_address, quantity }))
        }
        ModbusFunctionCode::ReadInputRegisters => {
            let (input, starting_address) = be_u16(input)?;
            let (input, quantity) = be_u16(input)?;
            Ok((input, ModbusMessageData::ReadInputRegisters { starting_address, quantity }))
        }
        ModbusFunctionCode::WriteSingleCoil => {
            let (input, output_address) = be_u16(input)?;
            let (input, output_value) = be_u16(input)?;
            Ok((input, ModbusMessageData::WriteSingleCoil { output_address, output_value }))
        }
        ModbusFunctionCode::WriteSingleRegister => {
            let (input, register_address) = be_u16(input)?;
            let (input, register_value) = be_u16(input)?;
            Ok((input, ModbusMessageData::WriteSingleRegister { register_address, register_value }))
        }
        ModbusFunctionCode::ReadExceptionStatus => Ok((input, ModbusMessageData::ReadExceptionStatus)),
        ModbusFunctionCode::Diagnostics => {
            let (input, sub_function) = be_u8(input)?;
            Ok((input, ModbusMessageData::Diagnostics { sub_function }))
        }
        ModbusFunctionCode::GetCommEventCounter => Ok((input, ModbusMessageData::GetCommEventCounter)),
        ModbusFunctionCode::GetCommEventLog => {
            let (input, status) = be_u8(input)?;
            let (input, event_count) = be_u16(input)?;
            Ok((input, ModbusMessageData::GetCommEventLog { status, event_count }))
        }
        ModbusFunctionCode::WriteMultipleCoils => {
            let (input, starting_address) = be_u16(input)?;
            let (input, quantity) = be_u16(input)?;
            let (input, output_values) = length_data(take(quantity / 8))(input)?;
            Ok((input, ModbusMessageData::WriteMultipleCoils { starting_address, quantity, output_values: output_values.to_vec() }))
        }
        ModbusFunctionCode::WriteMultipleRegisters => {
            let (input, starting_address) = be_u16(input)?;
            let (input, quantity) = be_u16(input)?;
            let (input, register_values) = length_data(take(quantity * 2))(input)?;
            let register_values = register_values.chunks(2).map(|chunk| ((chunk[0] as u16) << 8) | (chunk[1] as u16)).collect();
            Ok((input, ModbusMessageData::WriteMultipleRegisters { starting_address, quantity, register_values }))
        }
        ModbusFunctionCode::ReportServerId => {
            let (input, status) = be_u8(input)?;
            let (input, general_reference) = be_u8(input)?;
            Ok((input, ModbusMessageData::ReportServerId { status, general_reference }))
        }
        ModbusFunctionCode::ReadFileRecord => {
            let (input, file_number) = be_u16(input)?;
            let (input, record_number) = be_u16(input)?;
            let (input, record_length) = be_u16(input)?;
            Ok((input, ModbusMessageData::ReadFileRecord { file_number, record_number, record_length }))
        }
        ModbusFunctionCode::WriteFileRecord => {
            let (input, file_number) = be_u16(input)?;
            let (input, record_number) = be_u16(input)?;
            let (input, record_data) = length_data(take(2))(input)?;
            Ok((input, ModbusMessageData::WriteFileRecord { file_number, record_number, record_data: record_data.to_vec() }))
        }
        ModbusFunctionCode::MaskWriteRegister => {
            let (input, register_address) = be_u16(input)?;
            let (input, and_mask) = be_u16(input)?;
            let (input, or_mask) = be_u16(input)?;
            Ok((input, ModbusMessageData::MaskWriteRegister { register_address, and_mask, or_mask }))
        }
        ModbusFunctionCode::ReadWriteMultipleRegisters => {
            let (input, read_starting_address) = be_u16(input)?;
            let (input, read_quantity) = be_u16(input)?;
            let (input, write_starting_address) = be_u16(input)?;
            let (input, write_quantity) = be_u16(input)?;
            let (input, write_register_values) = length_data(take(write_quantity * 2))(input)?;
            let write_register_values = write_register_values.chunks(2).map(|chunk| ((chunk[0] as u16) << 8) | (chunk[1] as u16)).collect();
            Ok((input, ModbusMessageData::ReadWriteMultipleRegisters { read_starting_address, read_quantity, write_starting_address, write_quantity, write_register_values }))
        }
        ModbusFunctionCode::ReadUUID => Ok((input, ModbusMessageData::ReadUUID)),
        ModbusFunctionCode::Unknown(_) => {
            let (input, _data) = take(1)(input)?;
            Ok((input, ModbusMessageData::Unknown(_data.to_vec())))
        }
    }?;
    Ok((input, (slave_address, data)))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return;
    }
    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut input = Vec::new();
    file.read_to_end(&mut input).expect("Failed to read file");
    let result = parse_modbus_message(&input);
    match result {
        Ok((remaining, (slave_address, data))) => {
            println!("Slave Address: {}", slave_address);
            println!("Data: {:?}", data);
            if !remaining.is_empty() {
                println!("Remaining bytes: {:?}", remaining);
            }
        }
        Err(err) => {
            println!("Error parsing Modbus message: {:?}", err);
        }
    }
}