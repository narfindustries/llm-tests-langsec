use nom::{
    bytes::complete::{tag, take},
    combinator::map_res,
    number::complete::{be_u16, be_u8},
    sequence::tuple,
    IResult,
};
use std::{env, fs, io::Read};

#[derive(Debug)]
struct ModbusHeader {
    transaction_id: u16,
    protocol_id: u16,
    length: u16,
    unit_id: u8,
}

#[derive(Debug)]
enum ModbusFunctionCode {
    ReadCoils,
    ReadDiscreteInputs,
    ReadHoldingRegisters,
    ReadInputRegisters,
    WriteSingleCoil,
    WriteSingleRegister,
    WriteMultipleCoils,
    WriteMultipleRegisters,
    MaskWriteRegister,
    ReadWriteMultipleRegisters,
    Unknown(u8),
}

#[derive(Debug)]
struct ModbusRequest {
    header: ModbusHeader,
    function_code: ModbusFunctionCode,
    data: Vec<u8>,
}

#[derive(Debug)]
struct ModbusResponse {
    header: ModbusHeader,
    function_code: ModbusFunctionCode,
    data: Vec<u8>,
}

fn parse_modbus_header(input: &[u8]) -> IResult<&[u8], ModbusHeader> {
    let (input, (transaction_id, protocol_id, length, unit_id)) =
        tuple((be_u16, be_u16, be_u16, be_u8))(input)?;
    Ok((
        input,
        ModbusHeader {
            transaction_id,
            protocol_id,
            length,
            unit_id,
        },
    ))
}

fn parse_modbus_function_code(input: &[u8]) -> IResult<&[u8], ModbusFunctionCode> {
    let (input, code) = be_u8(input)?;
    let function_code = match code {
        0x01 => ModbusFunctionCode::ReadCoils,
        0x02 => ModbusFunctionCode::ReadDiscreteInputs,
        0x03 => ModbusFunctionCode::ReadHoldingRegisters,
        0x04 => ModbusFunctionCode::ReadInputRegisters,
        0x05 => ModbusFunctionCode::WriteSingleCoil,
        0x06 => ModbusFunctionCode::WriteSingleRegister,
        0x0F => ModbusFunctionCode::WriteMultipleCoils,
        0x10 => ModbusFunctionCode::WriteMultipleRegisters,
        0x16 => ModbusFunctionCode::MaskWriteRegister,
        0x17 => ModbusFunctionCode::ReadWriteMultipleRegisters,
        _ => ModbusFunctionCode::Unknown(code),
    };
    Ok((input, function_code))
}

fn parse_modbus_request(input: &[u8]) -> IResult<&[u8], ModbusRequest> {
    let (input, header) = parse_modbus_header(input)?;
    let (input, function_code) = parse_modbus_function_code(input)?;
    let (input, data) = take(header.length - 1)(input)?;
    Ok((
        input,
        ModbusRequest {
            header,
            function_code,
            data: data.to_vec(),
        },
    ))
}

fn parse_modbus_response(input: &[u8]) -> IResult<&[u8], ModbusResponse> {
    let (input, header) = parse_modbus_header(input)?;
    let (input, function_code) = parse_modbus_function_code(input)?;
    let (input, data) = take(header.length - 1)(input)?;
    Ok((
        input,
        ModbusResponse {
            header,
            function_code,
            data: data.to_vec(),
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let mut file = fs::File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    let result = parse_modbus_request(&buffer);
    match result {
        Ok((_, request)) => println!("Parsed Modbus Request: {:?}", request),
        Err(_) => {
            let result = parse_modbus_response(&buffer);
            match result {
                Ok((_, response)) => println!("Parsed Modbus Response: {:?}", response),
                Err(e) => eprintln!("Failed to parse Modbus message: {:?}", e),
            }
        }
    }
}