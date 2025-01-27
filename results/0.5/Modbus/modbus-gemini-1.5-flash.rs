use nom::{
    be::{be_u16, be_u8},
    bytes::complete::take,
    combinator::{map, map_res, opt, rest},
    error::ErrorKind,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
enum ModbusFunctionCode {
    ReadCoils(u16),
    ReadDiscreteInputs(u16),
    ReadHoldingRegisters(u16),
    ReadInputRegisters(u16),
    WriteSingleCoil(u16, bool),
    WriteSingleRegister(u16, u16),
    WriteMultipleCoils(u16, Vec<bool>),
    WriteMultipleRegisters(u16, Vec<u16>),
    ReadExceptionStatus,
    Diagnostic,
    // Add other function codes as needed...
}

fn parse_modbus_pdu(input: &[u8]) -> IResult<&[u8], ModbusFunctionCode> {
    let (input, function_code) = be_u8(input)?;

    match function_code {
        1 => {
            let (input, start_address) = be_u16(input)?;
            let (input, quantity) = be_u16(input)?;
            Ok((input, ModbusFunctionCode::ReadCoils(quantity)))
        }
        2 => {
            let (input, start_address) = be_u16(input)?;
            let (input, quantity) = be_u16(input)?;
            Ok((input, ModbusFunctionCode::ReadDiscreteInputs(quantity)))
        }
        3 => {
            let (input, start_address) = be_u16(input)?;
            let (input, quantity) = be_u16(input)?;
            Ok((input, ModbusFunctionCode::ReadHoldingRegisters(quantity)))
        }
        4 => {
            let (input, start_address) = be_u16(input)?;
            let (input, quantity) = be_u16(input)?;
            Ok((input, ModbusFunctionCode::ReadInputRegisters(quantity)))
        }
        5 => {
            let (input, start_address) = be_u16(input)?;
            let (input, value) = map(be_u16, |v| v > 0)(input)?;
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
            let coils = (0..quantity)
                .map(|i| (data[i as usize / 8] >> (7 - (i % 8))) & 1 != 0)
                .collect();
            Ok((input, ModbusFunctionCode::WriteMultipleCoils(start_address, coils)))
        }
        16 => {
            let (input, start_address) = be_u16(input)?;
            let (input, quantity) = be_u16(input)?;
            let (input, byte_count) = be_u8(input)?;
            let (input, data) = take(byte_count as usize)(input)?;
            let registers = (0..quantity)
                .map(|i| {
                    let offset = i * 2;
                    ((data[offset] as u16) << 8) | data[offset + 1] as u16
                })
                .collect();
            Ok((input, ModbusFunctionCode::WriteMultipleRegisters(start_address, registers)))
        }
        // Add other function codes as needed...
        _ => Err(nom::Err::Error(nom::error::Error {
            input,
            code: ErrorKind::Custom(1),
        })),
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
        Err(e) => println!("Failed to parse Modbus PDU: {:?}", e),
    }
}
