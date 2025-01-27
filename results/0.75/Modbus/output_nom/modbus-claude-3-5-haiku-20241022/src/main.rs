use nom::{
    bytes::complete::{tag, take},
    multi::many0,
    number::complete::{be_u16, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::env;
use std::fs;

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
}

fn parse_modbus_request(input: &[u8]) -> IResult<&[u8], ModbusRequest> {
    let (input, transaction_id) = be_u16(input)?;
    let (input, protocol_id) = be_u16(input)?;
    let (input, length) = be_u16(input)?;
    let (input, unit_id) = be_u8(input)?;
    let (input, function_code) = be_u8(input)?;
    let (input, data) = take(length as usize - 2)(input)?;

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
    let (input, transaction_id) = be_u16(input)?;
    let (input, protocol_id) = be_u16(input)?;
    let (input, length) = be_u16(input)?;
    let (input, unit_id) = be_u8(input)?;
    let (input, function_code) = be_u8(input)?;
    let (input, data) = take(length as usize - 2)(input)?;

    Ok((input, ModbusResponse {
        transaction_id,
        protocol_id,
        length,
        unit_id,
        function_code,
        data: data.to_vec(),
    }))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let input_file = &args[1];
    let input_data = fs::read(input_file)?;

    match parse_modbus_request(&input_data) {
        Ok((_, request)) => {
            println!("Parsed Modbus Request: {:?}", request);
        }
        Err(_) => {
            match parse_modbus_response(&input_data) {
                Ok((_, response)) => {
                    println!("Parsed Modbus Response: {:?}", response);
                }
                Err(e) => {
                    eprintln!("Failed to parse Modbus packet: {:?}", e);
                    std::process::exit(1);
                }
            }
        }
    }

    Ok(())
}