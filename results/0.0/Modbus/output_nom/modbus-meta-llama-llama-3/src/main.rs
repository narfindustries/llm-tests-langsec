use nom::{
    bytes::complete::{take},
    number::complete::{be_u8, be_u16},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug, PartialEq)]
enum FunctionCode {
    ReadCoilStatus = 0x01,
    ReadInputStatus = 0x02,
    ReadHoldingRegisters = 0x03,
    ReadInputRegisters = 0x04,
    WriteSingleCoil = 0x05,
    WriteSingleHoldingRegister = 0x06,
    WriteMultipleCoils = 0x0F,
    WriteMultipleHoldingRegisters = 0x10,
    ReportSlaveId = 0x11,
    ReadFileRecord = 0x14,
    WriteFileRecord = 0x15,
    MaskWriteRegister = 0x16,
    ReadWriteMultipleRegisters = 0x17,
    ReadFifoQueue = 0x18,
    EncapsulatedInterfaceTransport = 0x2B,
}

#[derive(Debug, PartialEq)]
enum ModbusMessage {
    ReadCoilStatus {
        slave_id: u8,
        function_code: FunctionCode,
        starting_address: u16,
        quantity_of_coils: u16,
        crc: u16,
    },
    ReadInputStatus {
        slave_id: u8,
        function_code: FunctionCode,
        starting_address: u16,
        quantity_of_inputs: u16,
        crc: u16,
    },
    ReadHoldingRegisters {
        slave_id: u8,
        function_code: FunctionCode,
        starting_address: u16,
        quantity_of_registers: u16,
        crc: u16,
    },
    ReadInputRegisters {
        slave_id: u8,
        function_code: FunctionCode,
        starting_address: u16,
        quantity_of_registers: u16,
        crc: u16,
    },
    WriteSingleCoil {
        slave_id: u8,
        function_code: FunctionCode,
        output_address: u16,
        output_value: u16,
        crc: u16,
    },
    WriteSingleHoldingRegister {
        slave_id: u8,
        function_code: FunctionCode,
        register_address: u16,
        register_value: u16,
        crc: u16,
    },
    WriteMultipleCoils {
        slave_id: u8,
        function_code: FunctionCode,
        starting_address: u16,
        quantity_of_outputs: u16,
        byte_count: u8,
        output_values: Vec<u8>,
        crc: u16,
    },
    WriteMultipleHoldingRegisters {
        slave_id: u8,
        function_code: FunctionCode,
        starting_address: u16,
        quantity_of_registers: u16,
        byte_count: u8,
        register_values: Vec<u8>,
        crc: u16,
    },
    ReportSlaveId {
        slave_id: u8,
        function_code: FunctionCode,
        byte_count: u8,
        slave_id_info: Vec<u8>,
        crc: u16,
    },
    ReadFileRecord {
        slave_id: u8,
        function_code: FunctionCode,
        file_number: u16,
        record_number: u16,
        record_length: u16,
        crc: u16,
    },
    WriteFileRecord {
        slave_id: u8,
        function_code: FunctionCode,
        file_number: u16,
        record_number: u16,
        record_length: u16,
        record_data: Vec<u8>,
        crc: u16,
    },
    MaskWriteRegister {
        slave_id: u8,
        function_code: FunctionCode,
        register_address: u16,
        and_mask: u16,
        or_mask: u16,
        crc: u16,
    },
    ReadWriteMultipleRegisters {
        slave_id: u8,
        function_code: FunctionCode,
        read_starting_address: u16,
        quantity_of_registers_to_read: u16,
        write_starting_address: u16,
        quantity_of_registers_to_write: u16,
        byte_count: u8,
        write_register_values: Vec<u8>,
        crc: u16,
    },
    ReadFifoQueue {
        slave_id: u8,
        function_code: FunctionCode,
        fifo_pointer_address: u16,
        crc: u16,
    },
    EncapsulatedInterfaceTransport {
        slave_id: u8,
        function_code: FunctionCode,
        interface_type: u8,
        interface_data: Vec<u8>,
        crc: u16,
    },
}

fn parse_modbus_message(input: &[u8]) -> IResult<&[u8], ModbusMessage> {
    let (input, slave_id) = be_u8(input)?;
    let (input, function_code) = be_u8(input)?;
    let function_code = match function_code {
        0x01 => FunctionCode::ReadCoilStatus,
        0x02 => FunctionCode::ReadInputStatus,
        0x03 => FunctionCode::ReadHoldingRegisters,
        0x04 => FunctionCode::ReadInputRegisters,
        0x05 => FunctionCode::WriteSingleCoil,
        0x06 => FunctionCode::WriteSingleHoldingRegister,
        0x0F => FunctionCode::WriteMultipleCoils,
        0x10 => FunctionCode::WriteMultipleHoldingRegisters,
        0x11 => FunctionCode::ReportSlaveId,
        0x14 => FunctionCode::ReadFileRecord,
        0x15 => FunctionCode::WriteFileRecord,
        0x16 => FunctionCode::MaskWriteRegister,
        0x17 => FunctionCode::ReadWriteMultipleRegisters,
        0x18 => FunctionCode::ReadFifoQueue,
        0x2B => FunctionCode::EncapsulatedInterfaceTransport,
        _ => panic!("Invalid function code"),
    };

    match function_code {
        FunctionCode::ReadCoilStatus => {
            let (input, starting_address) = be_u16(input)?;
            let (input, quantity_of_coils) = be_u16(input)?;
            let (input, crc) = be_u16(input)?;
            Ok((input, ModbusMessage::ReadCoilStatus { slave_id, function_code, starting_address, quantity_of_coils, crc }))
        }
        FunctionCode::ReadInputStatus => {
            let (input, starting_address) = be_u16(input)?;
            let (input, quantity_of_inputs) = be_u16(input)?;
            let (input, crc) = be_u16(input)?;
            Ok((input, ModbusMessage::ReadInputStatus { slave_id, function_code, starting_address, quantity_of_inputs, crc }))
        }
        FunctionCode::ReadHoldingRegisters => {
            let (input, starting_address) = be_u16(input)?;
            let (input, quantity_of_registers) = be_u16(input)?;
            let (input, crc) = be_u16(input)?;
            Ok((input, ModbusMessage::ReadHoldingRegisters { slave_id, function_code, starting_address, quantity_of_registers, crc }))
        }
        FunctionCode::ReadInputRegisters => {
            let (input, starting_address) = be_u16(input)?;
            let (input, quantity_of_registers) = be_u16(input)?;
            let (input, crc) = be_u16(input)?;
            Ok((input, ModbusMessage::ReadInputRegisters { slave_id, function_code, starting_address, quantity_of_registers, crc }))
        }
        FunctionCode::WriteSingleCoil => {
            let (input, output_address) = be_u16(input)?;
            let (input, output_value) = be_u16(input)?;
            let (input, crc) = be_u16(input)?;
            Ok((input, ModbusMessage::WriteSingleCoil { slave_id, function_code, output_address, output_value, crc }))
        }
        FunctionCode::WriteSingleHoldingRegister => {
            let (input, register_address) = be_u16(input)?;
            let (input, register_value) = be_u16(input)?;
            let (input, crc) = be_u16(input)?;
            Ok((input, ModbusMessage::WriteSingleHoldingRegister { slave_id, function_code, register_address, register_value, crc }))
        }
        FunctionCode::WriteMultipleCoils => {
            let (input, starting_address) = be_u16(input)?;
            let (input, quantity_of_outputs) = be_u16(input)?;
            let (input, byte_count) = be_u8(input)?;
            let (input, output_values) = take(byte_count as usize)(input)?;
            let (input, crc) = be_u16(input)?;
            Ok((input, ModbusMessage::WriteMultipleCoils { slave_id, function_code, starting_address, quantity_of_outputs, byte_count, output_values: output_values.to_vec(), crc }))
        }
        FunctionCode::WriteMultipleHoldingRegisters => {
            let (input, starting_address) = be_u16(input)?;
            let (input, quantity_of_registers) = be_u16(input)?;
            let (input, byte_count) = be_u8(input)?;
            let (input, register_values) = take(byte_count as usize)(input)?;
            let (input, crc) = be_u16(input)?;
            Ok((input, ModbusMessage::WriteMultipleHoldingRegisters { slave_id, function_code, starting_address, quantity_of_registers, byte_count, register_values: register_values.to_vec(), crc }))
        }
        FunctionCode::ReportSlaveId => {
            let (input, byte_count) = be_u8(input)?;
            let (input, slave_id_info) = take(byte_count as usize)(input)?;
            let (input, crc) = be_u16(input)?;
            Ok((input, ModbusMessage::ReportSlaveId { slave_id, function_code, byte_count, slave_id_info: slave_id_info.to_vec(), crc }))
        }
        FunctionCode::ReadFileRecord => {
            let (input, file_number) = be_u16(input)?;
            let (input, record_number) = be_u16(input)?;
            let (input, record_length) = be_u16(input)?;
            let (input, crc) = be_u16(input)?;
            Ok((input, ModbusMessage::ReadFileRecord { slave_id, function_code, file_number, record_number, record_length, crc }))
        }
        FunctionCode::WriteFileRecord => {
            let (input, file_number) = be_u16(input)?;
            let (input, record_number) = be_u16(input)?;
            let (input, record_length) = be_u16(input)?;
            let (input, record_data) = take(record_length as usize)(input)?;
            let (input, crc) = be_u16(input)?;
            Ok((input, ModbusMessage::WriteFileRecord { slave_id, function_code, file_number, record_number, record_length, record_data: record_data.to_vec(), crc }))
        }
        FunctionCode::MaskWriteRegister => {
            let (input, register_address) = be_u16(input)?;
            let (input, and_mask) = be_u16(input)?;
            let (input, or_mask) = be_u16(input)?;
            let (input, crc) = be_u16(input)?;
            Ok((input, ModbusMessage::MaskWriteRegister { slave_id, function_code, register_address, and_mask, or_mask, crc }))
        }
        FunctionCode::ReadWriteMultipleRegisters => {
            let (input, read_starting_address) = be_u16(input)?;
            let (input, quantity_of_registers_to_read) = be_u16(input)?;
            let (input, write_starting_address) = be_u16(input)?;
            let (input, quantity_of_registers_to_write) = be_u16(input)?;
            let (input, byte_count) = be_u8(input)?;
            let (input, write_register_values) = take(byte_count as usize)(input)?;
            let (input, crc) = be_u16(input)?;
            Ok((input, ModbusMessage::ReadWriteMultipleRegisters { slave_id, function_code, read_starting_address, quantity_of_registers_to_read, write_starting_address, quantity_of_registers_to_write, byte_count, write_register_values: write_register_values.to_vec(), crc }))
        }
        FunctionCode::ReadFifoQueue => {
            let (input, fifo_pointer_address) = be_u16(input)?;
            let (input, crc) = be_u16(input)?;
            Ok((input, ModbusMessage::ReadFifoQueue { slave_id, function_code, fifo_pointer_address, crc }))
        }
        FunctionCode::EncapsulatedInterfaceTransport => {
            let (input, interface_type) = be_u8(input)?;
            let (input, interface_data) = take(253usize)(input)?;
            let (input, crc) = be_u16(input)?;
            Ok((input, ModbusMessage::EncapsulatedInterfaceTransport { slave_id, function_code, interface_type, interface_data: interface_data.to_vec(), crc }))
        }
    }
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

    match parse_modbus_message(&input) {
        Ok((remaining, message)) => {
            println!("Parsed message: {:?}", message);
            if !remaining.is_empty() {
                println!("Warning: {} bytes remaining in input", remaining.len());
            }
        }
        Err(err) => {
            println!("Error parsing message: {:?}", err);
        }
    }
}