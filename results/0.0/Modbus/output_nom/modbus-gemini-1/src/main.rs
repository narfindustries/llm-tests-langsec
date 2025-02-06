use nom::{
    bytes::complete::take,
    combinator::map_res,
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
enum ModbusFunctionCode {
    ReadCoils(u16, u16),
    ReadDiscreteInputs(u16, u16),
    ReadHoldingRegisters(u16, u16),
    ReadInputRegisters(u16, u16),
    WriteSingleCoil(u16, bool),
    WriteSingleRegister(u16, u16),
    WriteMultipleCoils(u16, u16, Vec<u8>),
    WriteMultipleRegisters(u16, u16, Vec<u8>),
    Other(u8),
}

fn parse_modbus_pdu(input: &[u8]) -> IResult<&[u8], ModbusFunctionCode> {
    let (input, function_code) = be_u8(input)?;

    match function_code {
        1 => {
            let (input, start_address) = be_u16(input)?;
            let (input, quantity) = be_u16(input)?;
            Ok((input, ModbusFunctionCode::ReadCoils(start_address, quantity)))
        }
        2 => {
            let (input, start_address) = be_u16(input)?;
            let (input, quantity) = be_u16(input)?;
            Ok((input, ModbusFunctionCode::ReadDiscreteInputs(start_address, quantity)))
        }
        3 => {
            let (input, start_address) = be_u16(input)?;
            let (input, quantity) = be_u16(input)?;
            Ok((input, ModbusFunctionCode::ReadHoldingRegisters(start_address, quantity)))
        }
        4 => {
            let (input, start_address) = be_u16(input)?;
            let (input, quantity) = be_u16(input)?;
            Ok((input, ModbusFunctionCode::ReadInputRegisters(start_address, quantity)))
        }
        5 => {
            let (input, start_address) = be_u16(input)?;
            let (input, value) = map_res(take(1usize), |bytes: &[u8]| {
                Ok::<bool, nom::Err<nom::error::Error<&[u8]>>>(bytes[0] != 0)
            })(input)?;
            Ok((input, ModbusFunctionCode::WriteSingleCoil(start_address, value)))
        }
        6 => {
            let (input, start_address) = be_u16(input)?;
            let (input, value) = be_u16(input)?;
            Ok((input, ModbusFunctionCode::WriteSingleRegister(start_address, value)))
        }
        15 => {
            let (input, start_address) = be_u16(input)?;
            let (input, quantity) = be_u16(input)?;
            let (input, byte_count) = be_u8(input)?;
            let (input, data) = take(byte_count as usize)(input)?;
            Ok((
                input,
                ModbusFunctionCode::WriteMultipleCoils(start_address, quantity, data.to_vec()),
            ))
        }
        16 => {
            let (input, start_address) = be_u16(input)?;
            let (input, quantity) = be_u16(input)?;
            let (input, byte_count) = be_u8(input)?;
            let (input, data) = take(byte_count as usize)(input)?;
            Ok((
                input,
                ModbusFunctionCode::WriteMultipleRegisters(start_address, quantity, data.to_vec()),
            ))
        }
        _ => Ok((input, ModbusFunctionCode::Other(function_code))),
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

    match parse_modbus_pdu(&buffer) {
        Ok((_, pdu)) => println!("Parsed Modbus PDU: {:?}", pdu),
        Err(e) => println!("Error parsing Modbus PDU: {:?}", e),
    }
}
