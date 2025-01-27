use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    error::ErrorKind,
    multi::length_data,
    number::complete::{be_u8, be_u16, be_u16 as be_uint16},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

// Modbus Application Protocol Specification V1.1b3
// https://modbus.org/docs/Modbus_Application_Protocol_V1_1b3.pdf

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
    ReportServerId = 0x11,
    ReadFileRecord = 0x14,
    WriteFileRecord = 0x15,
    MaskWriteRegister = 0x16,
    ReadWriteMultipleRegisters = 0x17,
    ReadFirmware = 0x18,
    WriteFirmware = 0x19,
    ReadInformation = 0x2B,
    WriteInformation = 0x2C,
    ReadDeviceIdentification = 0x2D,
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

#[derive(Debug, PartialEq)]
enum ModbusResponse {
    ReadCoils {
        transaction_id: u16,
        protocol_id: u16,
        length: u16,
        unit_id: u8,
        function_code: ModbusFunctionCode,
        coil_status: Vec<bool>,
    },
    ReadDiscreteInputs {
        transaction_id: u16,
        protocol_id: u16,
        length: u16,
        unit_id: u8,
        function_code: ModbusFunctionCode,
        input_status: Vec<bool>,
    },
    ReadHoldingRegisters {
        transaction_id: u16,
        protocol_id: u16,
        length: u16,
        unit_id: u8,
        function_code: ModbusFunctionCode,
        register_values: Vec<u16>,
    },
    ReadInputRegisters {
        transaction_id: u16,
        protocol_id: u16,
        length: u16,
        unit_id: u8,
        function_code: ModbusFunctionCode,
        register_values: Vec<u16>,
    },
    WriteSingleCoil {
        transaction_id: u16,
        protocol_id: u16,
        length: u16,
        unit_id: u8,
        function_code: ModbusFunctionCode,
        coil_address: u16,
        coil_value: bool,
    },
    WriteSingleRegister {
        transaction_id: u16,
        protocol_id: u16,
        length: u16,
        unit_id: u8,
        function_code: ModbusFunctionCode,
        register_address: u16,
        register_value: u16,
    },
    ReadExceptionStatus {
        transaction_id: u16,
        protocol_id: u16,
        length: u16,
        unit_id: u8,
        function_code: ModbusFunctionCode,
        status: u8,
    },
    Diagnostics {
        transaction_id: u16,
        protocol_id: u16,
        length: u16,
        unit_id: u8,
        function_code: ModbusFunctionCode,
        sub_function: u16,
        data: Vec<u8>,
    },
    GetCommEventCounter {
        transaction_id: u16,
        protocol_id: u16,
        length: u16,
        unit_id: u8,
        function_code: ModbusFunctionCode,
        event_count: u16,
        event_log: Vec<u8>,
    },
    GetCommEventLog {
        transaction_id: u16,
        protocol_id: u16,
        length: u16,
        unit_id: u8,
        function_code: ModbusFunctionCode,
        status: u8,
        event_log: Vec<u8>,
    },
    WriteMultipleCoils {
        transaction_id: u16,
        protocol_id: u16,
        length: u16,
        unit_id: u8,
        function_code: ModbusFunctionCode,
        starting_address: u16,
        quantity: u16,
    },
    WriteMultipleRegisters {
        transaction_id: u16,
        protocol_id: u16,
        length: u16,
        unit_id: u8,
        function_code: ModbusFunctionCode,
        starting_address: u16,
        quantity: u16,
        register_values: Vec<u16>,
    },
    ReportServerId {
        transaction_id: u16,
        protocol_id: u16,
        length: u16,
        unit_id: u8,
        function_code: ModbusFunctionCode,
        server_id: u8,
        run_indicator_status: u8,
    },
    ReadFileRecord {
        transaction_id: u16,
        protocol_id: u16,
        length: u16,
        unit_id: u8,
        function_code: ModbusFunctionCode,
        file_number: u16,
        record_number: u16,
        record_data: Vec<u8>,
    },
    WriteFileRecord {
        transaction_id: u16,
        protocol_id: u16,
        length: u16,
        unit_id: u8,
        function_code: ModbusFunctionCode,
        file_number: u16,
        record_number: u16,
        record_data: Vec<u8>,
    },
    MaskWriteRegister {
        transaction_id: u16,
        protocol_id: u16,
        length: u16,
        unit_id: u8,
        function_code: ModbusFunctionCode,
        register_address: u16,
        and_mask: u16,
        or_mask: u16,
    },
    ReadWriteMultipleRegisters {
        transaction_id: u16,
        protocol_id: u16,
        length: u16,
        unit_id: u8,
        function_code: ModbusFunctionCode,
        read_starting_address: u16,
        quantity_to_read: u16,
        write_starting_address: u16,
        quantity_to_write: u16,
        write_register_values: Vec<u16>,
    },
    ReadFirmware {
        transaction_id: u16,
        protocol_id: u16,
        length: u16,
        unit_id: u8,
        function_code: ModbusFunctionCode,
        file_number: u16,
        record_number: u16,
        record_data: Vec<u8>,
    },
    WriteFirmware {
        transaction_id: u16,
        protocol_id: u16,
        length: u16,
        unit_id: u8,
        function_code: ModbusFunctionCode,
        file_number: u16,
        record_number: u16,
        record_data: Vec<u8>,
    },
    ReadInformation {
        transaction_id: u16,
        protocol_id: u16,
        length: u16,
        unit_id: u8,
        function_code: ModbusFunctionCode,
        information_type: u8,
        information_data: Vec<u8>,
    },
    WriteInformation {
        transaction_id: u16,
        protocol_id: u16,
        length: u16,
        unit_id: u8,
        function_code: ModbusFunctionCode,
        information_type: u8,
        information_data: Vec<u8>,
    },
    ReadDeviceIdentification {
        transaction_id: u16,
        protocol_id: u16,
        length: u16,
        unit_id: u8,
        function_code: ModbusFunctionCode,
        identifier: u8,
        object_id: u8,
        information_data: Vec<u8>,
    },
}

fn parse_modbus_request(i: &[u8]) -> IResult<&[u8], ModbusRequest> {
    let (i, transaction_id) = be_u16(i)?;
    let (i, protocol_id) = be_u16(i)?;
    let (i, length) = be_u16(i)?;
    let (i, unit_id) = be_u8(i)?;
    let (i, function_code) = map(be_u8, |x| match x {
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
        0x18 => ModbusFunctionCode::ReadFirmware,
        0x19 => ModbusFunctionCode::WriteFirmware,
        0x2B => ModbusFunctionCode::ReadInformation,
        0x2C => ModbusFunctionCode::WriteInformation,
        0x2D => ModbusFunctionCode::ReadDeviceIdentification,
        _ => panic!("Invalid function code"),
    })(i)?;
    Ok((i, ModbusRequest {
        transaction_id,
        protocol_id,
        length,
        unit_id,
        function_code,
    }))
}

fn parse_modbus_exception_response(i: &[u8]) -> IResult<&[u8], ModbusExceptionResponse> {
    let (i, transaction_id) = be_u16(i)?;
    let (i, protocol_id) = be_u16(i)?;
    let (i, length) = be_u16(i)?;
    let (i, unit_id) = be_u8(i)?;
    let (i, function_code) = be_u8(i)?;
    let (i, exception_code) = map(be_u8, |x| match x {
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
    })(i)?;
    Ok((i, ModbusExceptionResponse {
        transaction_id,
        protocol_id,
        length,
        unit_id,
        function_code,
        exception_code,
    }))
}

fn parse_modbus_response(i: &[u8]) -> IResult<&[u8], ModbusResponse> {
    let (i, transaction_id) = be_u16(i)?;
    let (i, protocol_id) = be_u16(i)?;
    let (i, length) = be_u16(i)?;
    let (i, unit_id) = be_u8(i)?;
    let (i, function_code) = be_u8(i)?;
    match function_code {
        0x01 => {
            let (i, coil_status) = take(length as usize - 2)(i)?;
            let coil_status: Vec<bool> = coil_status
                .iter()
                .flat_map(|x| {
                    (0..8)
                        .map(move |i| (x & (1 << i)) != 0)
                        .collect::<Vec<bool>>()
                })
                .collect();
            Ok((i, ModbusResponse::ReadCoils {
                transaction_id,
                protocol_id,
                length,
                unit_id,
                function_code: ModbusFunctionCode::ReadCoils,
                coil_status,
            }))
        }
        0x02 => {
            let (i, input_status) = take(length as usize - 2)(i)?;
            let input_status: Vec<bool> = input_status
                .iter()
                .flat_map(|x| {
                    (0..8)
                        .map(move |i| (x & (1 << i)) != 0)
                        .collect::<Vec<bool>>()
                })
                .collect();
            Ok((i, ModbusResponse::ReadDiscreteInputs {
                transaction_id,
                protocol_id,
                length,
                unit_id,
                function_code: ModbusFunctionCode::ReadDiscreteInputs,
                input_status,
            }))
        }
        0x03 => {
            let (i, register_values) = take(length as usize - 2)(i)?;
            let register_values: Vec<u16> = register_values
                .chunks(2)
                .map(|x| ((x[0] as u16) << 8) | (x[1] as u16))
                .collect();
            Ok((i, ModbusResponse::ReadHoldingRegisters {
                transaction_id,
                protocol_id,
                length,
                unit_id,
                function_code: ModbusFunctionCode::ReadHoldingRegisters,
                register_values,
            }))
        }
        0x04 => {
            let (i, register_values) = take(length as usize - 2)(i)?;
            let register_values: Vec<u16> = register_values
                .chunks(2)
                .map(|x| ((x[0] as u16) << 8) | (x[1] as u16))
                .collect();
            Ok((i, ModbusResponse::ReadInputRegisters {
                transaction_id,
                protocol_id,
                length,
                unit_id,
                function_code: ModbusFunctionCode::ReadInputRegisters,
                register_values,
            }))
        }
        0x05 => {
            let (i, coil_address) = be_u16(i)?;
            let (i, coil_value) = be_u16(i)?;
            Ok((i, ModbusResponse::WriteSingleCoil {
                transaction_id,
                protocol_id,
                length,
                unit_id,
                function_code: ModbusFunctionCode::WriteSingleCoil,
                coil_address,
                coil_value: coil_value == 0xFF00,
            }))
        }
        0x06 => {
            let (i, register_address) = be_u16(i)?;
            let (i, register_value) = be_u16(i)?;
            Ok((i, ModbusResponse::WriteSingleRegister {
                transaction_id,
                protocol_id,
                length,
                unit_id,
                function_code: ModbusFunctionCode::WriteSingleRegister,
                register_address,
                register_value,
            }))
        }
        0x07 => {
            let (i, status) = be_u8(i)?;
            Ok((i, ModbusResponse::ReadExceptionStatus {
                transaction_id,
                protocol_id,
                length,
                unit_id,
                function_code: ModbusFunctionCode::ReadExceptionStatus,
                status,
            }))
        }
        0x08 => {
            let (i, sub_function) = be_u16(i)?;
            let (i, data) = take(length as usize - 4)(i)?;
            Ok((i, ModbusResponse::Diagnostics {
                transaction_id,
                protocol_id,
                length,
                unit_id,
                function_code: ModbusFunctionCode::Diagnostics,
                sub_function,
                data: data.to_vec(),
            }))
        }
        0x0B => {
            let (i, event_count) = be_u16(i)?;
            let (i, event_log) = take(length as usize - 4)(i)?;
            Ok((i, ModbusResponse::GetCommEventCounter {
                transaction_id,
                protocol_id,
                length,
                unit_id,
                function_code: ModbusFunctionCode::GetCommEventCounter,
                event_count,
                event_log: event_log.to_vec(),
            }))
        }
        0x0C => {
            let (i, status) = be_u8(i)?;
            let (i, event_log) = take(length as usize - 3)(i)?;
            Ok((i, ModbusResponse::GetCommEventLog {
                transaction_id,
                protocol_id,
                length,
                unit_id,
                function_code: ModbusFunctionCode::GetCommEventLog,
                status,
                event_log: event_log.to_vec(),
            }))
        }
        0x0F => {
            let (i, starting_address) = be_u16(i)?;
            let (i, quantity) = be_u16(i)?;
            Ok((i, ModbusResponse::WriteMultipleCoils {
                transaction_id,
                protocol_id,
                length,
                unit_id,
                function_code: ModbusFunctionCode::WriteMultipleCoils,
                starting_address,
                quantity,
            }))
        }
        0x10 => {
            let (i, starting_address) = be_u16(i)?;
            let (i, quantity) = be_u16(i)?;
            let (i, register_values) = take(length as usize - 6)(i)?;
            let register_values: Vec<u16> = register_values
                .chunks(2)
                .map(|x| ((x[0] as u16) << 8) | (x[1] as u16))
                .collect();
            Ok((i, ModbusResponse::WriteMultipleRegisters {
                transaction_id,
                protocol_id,
                length,
                unit_id,
                function_code: ModbusFunctionCode::WriteMultipleRegisters,
                starting_address,
                quantity,
