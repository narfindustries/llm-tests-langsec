use std::fs::File;
use std::io::Read;
use std::env;
use nom::{
    IResult,
    bytes::complete::take,
    number::complete::{be_u8, be_u16},
    combinator::map_res,
    sequence::tuple,
};

#[derive(Debug)]
enum ModbusFunction {
    ReadCoils,
    ReadDiscreteInputs,
    ReadHoldingRegisters,
    ReadInputRegisters,
    WriteSingleCoil,
    WriteSingleRegister,
    WriteMultipleCoils,
    WriteMultipleRegisters,
    ReportSlaveID,
    Unknown(u8),
}

#[derive(Debug)]
struct ModbusPDU {
    function_code: ModbusFunction,
    data: Vec<u8>,
}

#[derive(Debug)]
struct ModbusRTU {
    address: u8,
    pdu: ModbusPDU,
    crc: u16,
}

#[derive(Debug)]
struct ModbusTCP {
    transaction_id: u16,
    protocol_id: u16,
    length: u16,
    unit_id: u8,
    pdu: ModbusPDU,
}

fn parse_function_code(input: &[u8]) -> IResult<&[u8], ModbusFunction> {
    let (input, code) = be_u8(input)?;
    let function = match code {
        1 => ModbusFunction::ReadCoils,
        2 => ModbusFunction::ReadDiscreteInputs,
        3 => ModbusFunction::ReadHoldingRegisters,
        4 => ModbusFunction::ReadInputRegisters,
        5 => ModbusFunction::WriteSingleCoil,
        6 => ModbusFunction::WriteSingleRegister,
        15 => ModbusFunction::WriteMultipleCoils,
        16 => ModbusFunction::WriteMultipleRegisters,
        17 => ModbusFunction::ReportSlaveID,
        _ => ModbusFunction::Unknown(code),
    };
    Ok((input, function))
}

fn parse_pdu(input: &[u8]) -> IResult<&[u8], ModbusPDU> {
    let (input, (function_code, data)) = tuple((parse_function_code, take(input.len())))(input)?;
    Ok((input, ModbusPDU { function_code, data: data.to_vec() }))
}

fn parse_modbus_rtu(input: &[u8]) -> IResult<&[u8], ModbusRTU> {
    let (input, (address, pdu, crc)) = tuple((be_u8, parse_pdu, be_u16))(input)?;
    Ok((input, ModbusRTU { address, pdu, crc }))
}

fn parse_modbus_tcp(input: &[u8]) -> IResult<&[u8], ModbusTCP> {
    let (input, (transaction_id, protocol_id, length, unit_id, pdu)) =
        tuple((be_u16, be_u16, be_u16, be_u8, parse_pdu))(input)?;
    Ok((input, ModbusTCP { transaction_id, protocol_id, length, unit_id, pdu }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_modbus_rtu(&buffer) {
        Ok((_, rtu)) => println!("Parsed Modbus RTU: {:?}", rtu),
        Err(_) => match parse_modbus_tcp(&buffer) {
            Ok((_, tcp)) => println!("Parsed Modbus TCP: {:?}", tcp),
            Err(e) => eprintln!("Failed to parse Modbus message: {:?}", e),
        },
    }
}