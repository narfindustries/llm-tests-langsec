use nom::{ 
    bytes::complete::take,
    number::complete::{be_u16, be_u8, le_u16},
    sequence::tuple,
    IResult, 
};
use std::fs;
use std::env;

#[derive(Debug)]
struct ModbusHeader {
    transaction_id: u16,
    protocol_id: u16,
    length: u16,
    unit_id: u8,
}

#[derive(Debug)]
struct ModbusPayload {
    function_code: u8,
    data: Vec<u8>,
}

#[derive(Debug)]
struct ModbusException {
    function_code: u8,
    exception_code: u8,
}

#[derive(Debug)]
enum ModbusFrame {
    Valid(ModbusHeader, ModbusPayload),
    Exception(ModbusHeader, ModbusException),
}

fn parse_modbus_header(input: &[u8]) -> IResult<&[u8], ModbusHeader> {
    let (input, (transaction_id, protocol_id, length, unit_id)) = 
        tuple((be_u16, be_u16, be_u16, be_u8))(input)?;
    Ok((input, ModbusHeader { transaction_id, protocol_id, length, unit_id }))
}

fn parse_modbus_payload(input: &[u8], length: u16) -> IResult<&[u8], ModbusPayload> {
    let (input, function_code) = be_u8(input)?;
    let (input, data) = take(length - 1)(input)?;
    Ok((input, ModbusPayload { function_code, data: data.to_vec() }))
}

fn parse_modbus_exception(input: &[u8]) -> IResult<&[u8], ModbusException> {
    let (input, (function_code, exception_code)) = tuple((be_u8, be_u8))(input)?;
    Ok((input, ModbusException { function_code, exception_code }))
}

fn parse_modbus_frame(input: &[u8]) -> IResult<&[u8], ModbusFrame> {
    let (input, header) = parse_modbus_header(input)?;
    if header.length > 0 {
        if header.length <= 2 {
            let (input, exception) = parse_modbus_exception(input)?;
            Ok((input, ModbusFrame::Exception(header, exception)))
        } else {
            let (input, payload) = parse_modbus_payload(input, header.length)?;
            Ok((input, ModbusFrame::Valid(header, payload)))
        }
    } else {
        Ok((input, ModbusFrame::Valid(header, ModbusPayload { function_code: 0, data: vec![] })))
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        return;
    }
    let file_path = &args[1];
    let data = fs::read(file_path).expect("Failed to read file");
    match parse_modbus_frame(&data) {
        Ok((_, frame)) => println!("{:#?}", frame),
        Err(e) => eprintln!("Failed to parse Modbus frame: {:?}", e),
    }
}