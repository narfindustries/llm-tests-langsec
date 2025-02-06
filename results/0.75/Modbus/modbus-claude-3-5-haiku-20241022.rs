use nom::{
    combinator::map,
    number::complete::{be_u8, be_u16},
    multi::count,
    IResult,
};
use std::env;
use std::fs;
use std::path::Path;

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
    Other(u8),
}

#[derive(Debug)]
struct ModbusRequest {
    transaction_id: u16,
    protocol_id: u16,
    length: u16,
    unit_id: u8,
    function: ModbusFunction,
    starting_address: Option<u16>,
    quantity: Option<u16>,
    data: Option<Vec<u8>>,
}

#[derive(Debug)]
struct ModbusResponse {
    transaction_id: u16,
    protocol_id: u16,
    length: u16,
    unit_id: u8,
    function: ModbusFunction,
    byte_count: Option<u8>,
    data: Option<Vec<u8>>,
    error_code: Option<u8>,
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
        x => ModbusFunction::Other(x),
    })(input)
}

fn parse_modbus_request(input: &[u8]) -> IResult<&[u8], ModbusRequest> {
    let (input, transaction_id) = be_u16(input)?;
    let (input, protocol_id) = be_u16(input)?;
    let (input, length) = be_u16(input)?;
    let (input, unit_id) = be_u8(input)?;
    let (input, function) = parse_function_code(input)?;

    let result = match function {
        ModbusFunction::ReadCoils | 
        ModbusFunction::ReadDiscreteInputs | 
        ModbusFunction::ReadHoldingRegisters | 
        ModbusFunction::ReadInputRegisters => {
            let (remaining, addr) = be_u16(input)?;
            let (remaining, qty) = be_u16(remaining)?;
            (remaining, Some(addr), Some(qty), None)
        },
        ModbusFunction::WriteSingleCoil | 
        ModbusFunction::WriteSingleRegister => {
            let (remaining, addr) = be_u16(input)?;
            let (remaining, val) = be_u16(remaining)?;
            (remaining, Some(addr), None, Some(vec![val as u8]))
        },
        ModbusFunction::WriteMultipleCoils | 
        ModbusFunction::WriteMultipleRegisters => {
            let (remaining, addr) = be_u16(input)?;
            let (remaining, qty) = be_u16(remaining)?;
            let (remaining, byte_count) = be_u8(remaining)?;
            let (remaining, data_bytes) = count(be_u8, byte_count as usize)(remaining)?;
            (remaining, Some(addr), Some(qty), Some(data_bytes))
        },
        ModbusFunction::Other(_) => (input, None, None, None)
    };

    Ok((result.0, ModbusRequest {
        transaction_id,
        protocol_id,
        length,
        unit_id,
        function,
        starting_address: result.1,
        quantity: result.2,
        data: result.3,
    }))
}

fn parse_modbus_response(input: &[u8]) -> IResult<&[u8], ModbusResponse> {
    let (input, transaction_id) = be_u16(input)?;
    let (input, protocol_id) = be_u16(input)?;
    let (input, length) = be_u16(input)?;
    let (input, unit_id) = be_u8(input)?;
    let (input, function) = parse_function_code(input)?;

    let result = match function {
        ModbusFunction::ReadCoils | 
        ModbusFunction::ReadDiscreteInputs | 
        ModbusFunction::ReadHoldingRegisters | 
        ModbusFunction::ReadInputRegisters => {
            let (remaining, bc) = be_u8(input)?;
            let (remaining, data_bytes) = count(be_u8, bc as usize)(remaining)?;
            (remaining, Some(bc), Some(data_bytes), None)
        },
        ModbusFunction::WriteSingleCoil | 
        ModbusFunction::WriteSingleRegister | 
        ModbusFunction::WriteMultipleCoils | 
        ModbusFunction::WriteMultipleRegisters => {
            let (remaining, _) = be_u16(input)?;
            let (remaining, _) = be_u16(remaining)?;
            (remaining, None, None, None)
        },
        ModbusFunction::Other(code) if code & 0x80 != 0 => {
            let (remaining, error) = be_u8(input)?;
            (remaining, None, None, Some(error))
        },
        _ => (input, None, None, None)
    };

    Ok((result.0, ModbusResponse {
        transaction_id,
        protocol_id,
        length,
        unit_id,
        function,
        byte_count: result.1,
        data: result.2,
        error_code: result.3,
    }))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let file_path = Path::new(&args[1]);
    let input = fs::read(file_path)?;

    match parse_modbus_request(&input) {
        Ok((_, request)) => {
            println!("Parsed Modbus Request: {:?}", request);
        },
        Err(_) => {
            match parse_modbus_response(&input) {
                Ok((_, response)) => {
                    println!("Parsed Modbus Response: {:?}", response);
                },
                Err(e) => {
                    eprintln!("Failed to parse Modbus message: {:?}", e);
                    std::process::exit(1);
                }
            }
        }
    }

    Ok(())
}