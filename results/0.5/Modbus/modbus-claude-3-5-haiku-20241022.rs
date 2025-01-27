use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::count,
    number::complete::{be_u16, be_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct ModbusRequest {
    transaction_id: u16,
    protocol_id: u16,
    length: u16,
    unit_id: u8,
    function_code: u8,
    data: Vec<u8>,
}

#[derive(Debug)]
struct ModbusResponse {
    transaction_id: u16,
    protocol_id: u16,
    length: u16,
    unit_id: u8,
    function_code: u8,
    data: Vec<u8>,
    exception_code: Option<u8>,
}

fn parse_modbus_request(input: &[u8]) -> IResult<&[u8], ModbusRequest> {
    let (input, (transaction_id, protocol_id, length, unit_id, function_code)) = tuple((
        be_u16,
        be_u16,
        be_u16,
        be_u8,
        be_u8,
    ))(input)?;

    let (input, data) = take(length - 2)(input)?;

    Ok((input, ModbusRequest {
        transaction_id,
        protocol_id,
        length,
        unit_id,
        function_code,
        data: data.to_vec(),
    }))
}

fn parse_modbus_response(input: &[u8]) -> IResult<&[u8], ModbusResponse> {
    let (input, (transaction_id, protocol_id, length, unit_id, function_code)) = tuple((
        be_u16,
        be_u16,
        be_u16,
        be_u8,
        be_u8,
    ))(input)?;

    let (input, exception_code) = opt(be_u8)(input)?;

    let (input, data) = take(length - 2 - (if exception_code.is_some() { 1 } else { 0 }))(input)?;

    Ok((input, ModbusResponse {
        transaction_id,
        protocol_id,
        length,
        unit_id,
        function_code,
        data: data.to_vec(),
        exception_code,
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

    match parse_modbus_request(&buffer) {
        Ok((_, request)) => {
            println!("Parsed Modbus Request: {:?}", request);
        }
        Err(_) => {
            match parse_modbus_response(&buffer) {
                Ok((_, response)) => {
                    println!("Parsed Modbus Response: {:?}", response);
                }
                Err(e) => {
                    eprintln!("Failed to parse Modbus message: {:?}", e);
                    std::process::exit(1);
                }
            }
        }
    }

    Ok(())
}