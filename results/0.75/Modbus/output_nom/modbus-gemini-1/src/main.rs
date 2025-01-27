use nom::{
    be::*,
    bytes::complete::*,
    combinator::*,
    error::*,
    multi::*,
    number::complete::*,
};
use std::env;
use std::fs::File;
use std::io::Read;

// Modbus function codes
#[derive(Debug, PartialEq, Eq)]
enum FunctionCode {
    ReadCoils(u8),
    ReadDiscreteInputs(u8),
    ReadHoldingRegisters(u8),
    ReadInputRegisters(u8),
    WriteSingleCoil(u16),
    WriteSingleRegister(u16),
    WriteMultipleCoils(u16, u16),
    WriteMultipleRegisters(u16, u16),
    ReadWriteMultipleRegisters(u16, u16, u16),
    Other(u8),
}

// Modbus PDU (Protocol Data Unit)
#[derive(Debug, PartialEq, Eq)]
struct ModbusPdu {
    function_code: FunctionCode,
    data: Vec<u8>,
}


fn parse_function_code(input: &[u8]) -> nom::IResult<&[u8], FunctionCode> {
    let (input, function_code) = u8(input)?;
    match function_code {
        1 => map(u8, |x| FunctionCode::ReadCoils(x))(input),
        2 => map(u8, |x| FunctionCode::ReadDiscreteInputs(x))(input),
        3 => map(u8, |x| FunctionCode::ReadHoldingRegisters(x))(input),
        4 => map(u8, |x| FunctionCode::ReadInputRegisters(x))(input),
        5 => map(le_u16, |x| FunctionCode::WriteSingleCoil(x))(input),
        6 => map(le_u16, |x| FunctionCode::WriteSingleRegister(x))(input),
        15 => {
            let (input, start) = le_u16(input)?;
            let (input, quantity) = le_u16(input)?;
            map(
                pair(le_u16, take_until_and_consume), |(byte_count, _)| {
                    FunctionCode::WriteMultipleCoils(start, quantity)
                },
            )(input)
        }
        16 => {
            let (input, start) = le_u16(input)?;
            let (input, quantity) = le_u16(input)?;
            map(
                pair(le_u16, take_until_and_consume), |(byte_count, _)| {
                    FunctionCode::WriteMultipleRegisters(start, quantity)
                },
            )(input)
        }
        23 => {
            let (input, read_start) = le_u16(input)?;
            let (input, read_quantity) = le_u16(input)?;
            let (input, write_start) = le_u16(input)?;
            let (input, write_quantity) = le_u16(input)?;
            map(take_until_and_consume, |_| {
                FunctionCode::ReadWriteMultipleRegisters(read_start, read_quantity, write_quantity)
            })(input)
        }
        code => Ok((input, FunctionCode::Other(code))),
    }
}


fn parse_modbus_pdu(input: &[u8]) -> nom::IResult<&[u8], ModbusPdu> {
    let (input, function_code) = parse_function_code(input)?;
    let (input, data) = match function_code {
        FunctionCode::ReadCoils(..) | FunctionCode::ReadDiscreteInputs(..) | FunctionCode::ReadHoldingRegisters(..) | FunctionCode::ReadInputRegisters(..) => {
            let (input, byte_count) = u8(input)?;
            let (input, data) = take(byte_count as usize)(input)?;
            (input, data.to_vec())
        }
        FunctionCode::WriteSingleCoil(..) | FunctionCode::WriteSingleRegister(..) => {
            (input, Vec::new())
        }
        FunctionCode::WriteMultipleCoils(..) | FunctionCode::WriteMultipleRegisters(..) | FunctionCode::ReadWriteMultipleRegisters(..) => {
            let (input, byte_count) = u8(input)?;
            let (input, data) = take(byte_count as usize)(input)?;
            (input, data.to_vec())
        }
        FunctionCode::Other(..) => (input, Vec::new()),
    };
    Ok((input, ModbusPdu { function_code, data }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_modbus_pdu(&buffer) {
        Ok((_, pdu)) => println!("{:?}", pdu),
        Err(e) => eprintln!("Error parsing Modbus PDU: {:?}", e),
    }
}
