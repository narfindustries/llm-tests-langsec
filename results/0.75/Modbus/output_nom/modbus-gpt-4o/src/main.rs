use nom::{
    bytes::complete::take,
    combinator::map,
    number::complete::{be_u16, be_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

// Modbus Function Codes
const READ_COILS: u8 = 0x01;
const READ_DISCRETE_INPUTS: u8 = 0x02;
const READ_HOLDING_REGISTERS: u8 = 0x03;
const READ_INPUT_REGISTERS: u8 = 0x04;
const WRITE_SINGLE_COIL: u8 = 0x05;
const WRITE_SINGLE_REGISTER: u8 = 0x06;
const DIAGNOSTICS: u8 = 0x08;

#[derive(Debug)]
enum ModbusFunction {
    ReadCoils(u16, u16),
    ReadDiscreteInputs(u16, u16),
    ReadHoldingRegisters(u16, u16),
    ReadInputRegisters(u16, u16),
    WriteSingleCoil(u16, u16),
    WriteSingleRegister(u16, u16),
    Diagnostics(u16, Vec<u8>),
    Unknown(u8, Vec<u8>),
}

#[derive(Debug)]
struct ModbusPDU {
    function_code: u8,
    data: ModbusFunction,
}

fn parse_modbus_pdu(input: &[u8]) -> IResult<&[u8], ModbusPDU> {
    let (input, function_code) = be_u8(input)?;
    let (input, data) = match function_code {
        READ_COILS | READ_DISCRETE_INPUTS | READ_HOLDING_REGISTERS | READ_INPUT_REGISTERS => {
            let (input, (starting_address, quantity)) = tuple((be_u16, be_u16))(input)?;
            let data = match function_code {
                READ_COILS => ModbusFunction::ReadCoils(starting_address, quantity),
                READ_DISCRETE_INPUTS => ModbusFunction::ReadDiscreteInputs(starting_address, quantity),
                READ_HOLDING_REGISTERS => ModbusFunction::ReadHoldingRegisters(starting_address, quantity),
                READ_INPUT_REGISTERS => ModbusFunction::ReadInputRegisters(starting_address, quantity),
                _ => unreachable!(),
            };
            (input, data)
        }
        WRITE_SINGLE_COIL => {
            let (input, (address, value)) = tuple((be_u16, be_u16))(input)?;
            (input, ModbusFunction::WriteSingleCoil(address, value))
        }
        WRITE_SINGLE_REGISTER => {
            let (input, (address, value)) = tuple((be_u16, be_u16))(input)?;
            (input, ModbusFunction::WriteSingleRegister(address, value))
        }
        DIAGNOSTICS => {
            let (input, (sub_function, data_length)) = tuple((be_u16, be_u16))(input)?;
            let (input, data) = take(data_length)(input)?;
            (input, ModbusFunction::Diagnostics(sub_function, data.to_vec()))
        }
        _ => {
            let data_length = input.len();
            let (input, data) = take(data_length)(input)?;
            (input, ModbusFunction::Unknown(function_code, data.to_vec()))
        }
    };
    Ok((input, ModbusPDU { function_code, data }))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_modbus_pdu(&buffer) {
        Ok((_, pdu)) => println!("Parsed Modbus PDU: {:?}", pdu),
        Err(err) => eprintln!("Failed to parse Modbus PDU: {:?}", err),
    }

    Ok(())
}