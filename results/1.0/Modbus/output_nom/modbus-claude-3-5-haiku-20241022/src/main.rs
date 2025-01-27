use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{count, many0},
    number::complete::{be_u8, be_u16, be_u32},
    sequence::{preceded, tuple},
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
    // Add other Modbus function codes
}

#[derive(Debug)]
struct ModbusRequest {
    transaction_id: u16,
    protocol_id: u16,
    length: u16,
    unit_id: u8,
    function_code: ModbusFunction,
    data: Vec<u8>,
}

fn parse_modbus_function(input: &[u8]) -> IResult<&[u8], ModbusFunction> {
    map(be_u8, |code| match code {
        0x01 => ModbusFunction::ReadCoils,
        0x02 => ModbusFunction::ReadDiscreteInputs,
        0x03 => ModbusFunction::ReadHoldingRegisters,
        0x04 => ModbusFunction::ReadInputRegisters,
        0x05 => ModbusFunction::WriteSingleCoil,
        0x06 => ModbusFunction::WriteSingleRegister,
        0x0F => ModbusFunction::WriteMultipleCoils,
        0x10 => ModbusFunction::WriteMultipleRegisters,
        _ => panic!("Unknown Modbus function code"),
    })(input)
}

fn parse_modbus_request(input: &[u8]) -> IResult<&[u8], ModbusRequest> {
    map(
        tuple((
            be_u16,   // transaction_id
            be_u16,   // protocol_id
            be_u16,   // length
            be_u8,    // unit_id
            parse_modbus_function,
            many0(be_u8),
        )),
        |(transaction_id, protocol_id, length, unit_id, function_code, data)| ModbusRequest {
            transaction_id,
            protocol_id,
            length,
            unit_id,
            function_code,
            data,
        },
    )(input)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <modbus_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1]).expect("Could not open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Could not read file");

    match parse_modbus_request(&buffer) {
        Ok((_, request)) => println!("Parsed Modbus Request: {:?}", request),
        Err(e) => eprintln!("Error parsing Modbus request: {:?}", e),
    }
}