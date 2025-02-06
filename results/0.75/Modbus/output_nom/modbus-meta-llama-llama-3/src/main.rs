use nom::{
    bytes::complete::{take},
    combinator::{map},
    IResult,
};
use std::{env, fs};

// Define the Modbus message structure
#[derive(Debug)]
struct ModbusMessage {
    address: u8,
    function_code: u8,
    data: Vec<u8>,
}

// Define the Modbus exception structure
#[derive(Debug)]
struct ModbusException {
    function_code: u8,
    exception_code: u8,
}

// Parser for the address field
fn address(input: &[u8]) -> IResult<&[u8], u8> {
    map(take(1usize), |address: &[u8]| address[0])(input)
}

// Parser for the function code field
fn function_code(input: &[u8]) -> IResult<&[u8], u8> {
    map(take(1usize), |function_code: &[u8]| function_code[0])(input)
}

// Parser for the data field
fn data(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    map(
        take(252usize),
        |data: &[u8]| data.to_vec(),
    )(input)
}

// Parser for the exception code field
fn exception_code(input: &[u8]) -> IResult<&[u8], u8> {
    map(take(1usize), |exception_code: &[u8]| exception_code[0])(input)
}

// Parser for the Modbus message
fn modbus_message(input: &[u8]) -> IResult<&[u8], ModbusMessage> {
    let (input, _address) = address(input)?;
    let (input, function_code) = function_code(input)?;
    let (input, data) = data(input)?;
    Ok((input, ModbusMessage { address: _address, function_code, data }))
}

// Parser for the Modbus exception
fn modbus_exception(input: &[u8]) -> IResult<&[u8], ModbusException> {
    let (input, function_code) = function_code(input)?;
    let (input, exception_code) = exception_code(input)?;
    Ok((input, ModbusException { function_code, exception_code }))
}

// Parser for the Modbus message or exception
fn modbus(input: &[u8]) -> IResult<&[u8], (u8, Vec<u8>)> {
    let (input, _address) = address(input)?;
    let (input, function_code) = function_code(input)?;
    match function_code {
        1..=127 => {
            let (input, data) = data(input)?;
            Ok((input, (function_code, data)))
        }
        _ => {
            let (input, exception_code) = exception_code(input)?;
            Ok((input, (function_code, vec![exception_code])))
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <input_file>", args[0]);
    }
    let input_file = &args[1];
    let input_data = fs::read(input_file).expect("Failed to read input file");
    match modbus(&input_data) {
        Ok((remaining, (function_code, data))) => {
            println!("Function Code: {}", function_code);
            println!("Data: {:?}", data);
            println!("Remaining: {:?}", remaining);
        }
        Err(err) => {
            println!("Error: {:?}", err);
        }
    }
}