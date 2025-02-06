use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt},
    number::complete::{be_u16, be_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
enum ModbusFunction {
    ReadCoils(u16, u16),
    ReadInputRegisters(u16, u16),
    ReadHoldingRegisters(u16, u16),
    WriteSingleCoil(u16, bool),
    WriteSingleRegister(u16, u16),
    WriteMultipleCoils(u16, u16, Vec<bool>),
    WriteMultipleRegisters(u16, u16, Vec<u16>),
    ReadException(u8),
    // Add other functions as needed...
}


fn modbus_pdu(input: &[u8]) -> IResult<&[u8], ModbusFunction> {
    let (input, function_code) = be_u8(input)?;

    match function_code {
        1 => {
            let (input, (start_addr, quantity)) = tuple((be_u16, be_u16))(input)?;
            Ok((input, ModbusFunction::ReadCoils(start_addr, quantity)))
        }
        2 => {
            let (input, (start_addr, quantity)) = tuple((be_u16, be_u16))(input)?;
            Ok((input, ModbusFunction::ReadInputRegisters(start_addr, quantity)))
        }
        3 => {
            let (input, (start_addr, quantity)) = tuple((be_u16, be_u16))(input)?;
            Ok((input, ModbusFunction::ReadHoldingRegisters(start_addr, quantity)))
        }
        5 => {
            let (input, (start_addr, value)) = tuple((be_u16, be_u16))(input)?;
            Ok((input, ModbusFunction::WriteSingleCoil(start_addr, value > 0)))
        }
        6 => {
            let (input, (start_addr, value)) = tuple((be_u16, be_u16))(input)?;
            Ok((input, ModbusFunction::WriteSingleRegister(start_addr, value)))
        }
        15 => {
            let (input, (start_addr, quantity, byte_count)) = tuple((be_u16, be_u16, be_u8))(input)?;
            let (input, data) = take(byte_count as usize)(input)?;
            let coils = data.iter().flat_map(|b| (0..8).map(move |i| (b >> i) & 1 != 0)).collect();
            Ok((input, ModbusFunction::WriteMultipleCoils(start_addr, quantity, coils)))

        }
        16 => {
            let (input, (start_addr, quantity, byte_count)) = tuple((be_u16, be_u16, be_u8))(input)?;
            let (input, data) = take(byte_count as usize)(input)?;
            let registers: Vec<u16> = data.chunks(2).map(|chunk| u16::from_be_bytes([chunk[0], chunk[1]])).collect();
            Ok((input, ModbusFunction::WriteMultipleRegisters(start_addr, quantity, registers)))
        }
        exception_code => Ok((input, ModbusFunction::ReadException(exception_code))),

    }
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match modbus_pdu(&buffer) {
        Ok((_, pdu)) => println!("Parsed Modbus PDU: {:?}", pdu),
        Err(e) => println!("Failed to parse Modbus PDU: {:?}", e),
    }
}
