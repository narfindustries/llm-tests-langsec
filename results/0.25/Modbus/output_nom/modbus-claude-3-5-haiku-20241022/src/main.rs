use nom::{
    bytes::complete::{take, tag},
    combinator::{map, opt},
    multi::{count, many0},
    number::complete::{be_u8, be_u16, be_u32},
    sequence::{tuple, preceded},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

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
    FileRecordRead,
    FileRecordWrite,
    EncapsulatedInterface,
}

#[derive(Debug)]
struct ModbusFrame {
    transaction_id: u16,
    protocol_id: u16,
    length: u16,
    unit_id: u8,
    function_code: ModbusFunction,
    data: Option<Vec<u8>>,
    crc: u16,
}

fn parse_function_code(input: &[u8]) -> IResult<&[u8], ModbusFunction> {
    map(be_u8, |code| match code {
        0x01 => ModbusFunction::ReadCoils,
        0x02 => ModbusFunction::ReadDiscreteInputs,
        0x03 => ModbusFunction::ReadHoldingRegisters,
        0x04 => ModbusFunction::ReadInputRegisters,
        0x05 => ModbusFunction::WriteSingleCoil,
        0x06 => ModbusFunction::WriteSingleRegister,
        0x0F => ModbusFunction::WriteMultipleCoils,
        0x10 => ModbusFunction::WriteMultipleRegisters,
        0x14..=0x17 => ModbusFunction::FileRecordRead,
        0x2B => ModbusFunction::EncapsulatedInterface,
        _ => panic!("Unknown function code"),
    })(input)
}

fn parse_modbus_frame(input: &[u8]) -> IResult<&[u8], ModbusFrame> {
    let (input, transaction_id) = be_u16(input)?;
    let (input, protocol_id) = be_u16(input)?;
    let (input, length) = be_u16(input)?;
    let (input, unit_id) = be_u8(input)?;
    let (input, function_code) = parse_function_code(input)?;
    
    let (input, data) = opt(take(length as usize - 2))(input)?;
    let (input, crc) = be_u16(input)?;

    Ok((input, ModbusFrame {
        transaction_id,
        protocol_id,
        length,
        unit_id,
        function_code,
        data: data.map(|d| d.to_vec()),
        crc,
    }))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_modbus_frame(&buffer) {
        Ok((_, frame)) => {
            println!("Parsed Modbus Frame: {:?}", frame);
        },
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}