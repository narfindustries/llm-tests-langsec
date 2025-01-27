use nom::{
    IResult,
    bytes::complete::{tag, take},
    number::complete::{be_u8, be_u16},
    sequence::tuple,
    multi::count,
};
use std::env;
use std::fs;

#[derive(Debug)]
struct ModbusADU {
    transaction_id: u16,
    protocol_id: u16,
    length: u16,
    unit_id: u8,
    pdu: ModbusPDU,
}

#[derive(Debug)]
enum ModbusPDU {
    ReadCoils(ModbusRequest),
    ReadDiscreteInputs(ModbusRequest),
    ReadHoldingRegisters(ModbusRequest),
    ReadInputRegisters(ModbusRequest),
    WriteSingleCoil(ModbusWriteRequest),
    WriteSingleRegister(ModbusWriteRequest),
    WriteMultipleCoils(ModbusWriteMultipleRequest),
    WriteMultipleRegisters(ModbusWriteMultipleRequest),
    Response(ModbusResponse),
    Exception(ModbusException),
}

#[derive(Debug)]
struct ModbusRequest {
    starting_address: u16,
    quantity: u16,
}

#[derive(Debug)]
struct ModbusWriteRequest {
    address: u16,
    value: u16,
}

#[derive(Debug)]
struct ModbusWriteMultipleRequest {
    starting_address: u16,
    quantity: u16,
    byte_count: u8,
    values: Vec<u8>,
}

#[derive(Debug)]
struct ModbusResponse {
    byte_count: u8,
    values: Vec<u8>,
}

#[derive(Debug)]
struct ModbusException {
    exception_code: u8,
}

fn parse_modbus_request(input: &[u8]) -> IResult<&[u8], ModbusRequest> {
    let (input, (starting_address, quantity)) = tuple((be_u16, be_u16))(input)?;
    Ok((input, ModbusRequest { starting_address, quantity }))
}

fn parse_modbus_write_request(input: &[u8]) -> IResult<&[u8], ModbusWriteRequest> {
    let (input, (address, value)) = tuple((be_u16, be_u16))(input)?;
    Ok((input, ModbusWriteRequest { address, value }))
}

fn parse_modbus_write_multiple_request(input: &[u8]) -> IResult<&[u8], ModbusWriteMultipleRequest> {
    let (input, (starting_address, quantity, byte_count)) = tuple((be_u16, be_u16, be_u8))(input)?;
    let (input, values) = count(be_u8, byte_count as usize)(input)?;
    Ok((input, ModbusWriteMultipleRequest {
        starting_address,
        quantity,
        byte_count,
        values,
    }))
}

fn parse_modbus_response(input: &[u8]) -> IResult<&[u8], ModbusResponse> {
    let (input, byte_count) = be_u8(input)?;
    let (input, values) = count(be_u8, byte_count as usize)(input)?;
    Ok((input, ModbusResponse { byte_count, values }))
}

fn parse_modbus_exception(input: &[u8]) -> IResult<&[u8], ModbusException> {
    let (input, exception_code) = be_u8(input)?;
    Ok((input, ModbusException { exception_code }))
}

fn parse_modbus_pdu(input: &[u8]) -> IResult<&[u8], ModbusPDU> {
    let (input, function_code) = be_u8(input)?;
    
    match function_code {
        0x01 => {
            let (input, request) = parse_modbus_request(input)?;
            Ok((input, ModbusPDU::ReadCoils(request)))
        },
        0x02 => {
            let (input, request) = parse_modbus_request(input)?;
            Ok((input, ModbusPDU::ReadDiscreteInputs(request)))
        },
        0x03 => {
            let (input, request) = parse_modbus_request(input)?;
            Ok((input, ModbusPDU::ReadHoldingRegisters(request)))
        },
        0x04 => {
            let (input, request) = parse_modbus_request(input)?;
            Ok((input, ModbusPDU::ReadInputRegisters(request)))
        },
        0x05 => {
            let (input, request) = parse_modbus_write_request(input)?;
            Ok((input, ModbusPDU::WriteSingleCoil(request)))
        },
        0x06 => {
            let (input, request) = parse_modbus_write_request(input)?;
            Ok((input, ModbusPDU::WriteSingleRegister(request)))
        },
        0x0F => {
            let (input, request) = parse_modbus_write_multiple_request(input)?;
            Ok((input, ModbusPDU::WriteMultipleCoils(request)))
        },
        0x10 => {
            let (input, request) = parse_modbus_write_multiple_request(input)?;
            Ok((input, ModbusPDU::WriteMultipleRegisters(request)))
        },
        fc if (fc & 0x80) != 0 => {
            let (input, exception) = parse_modbus_exception(input)?;
            Ok((input, ModbusPDU::Exception(exception)))
        },
        _ => {
            let (input, response) = parse_modbus_response(input)?;
            Ok((input, ModbusPDU::Response(response)))
        },
    }
}

fn parse_modbus_adu(input: &[u8]) -> IResult<&[u8], ModbusADU> {
    let (input, (transaction_id, protocol_id, length, unit_id)) = 
        tuple((be_u16, be_u16, be_u16, be_u8))(input)?;
    let (input, pdu) = parse_modbus_pdu(input)?;
    
    Ok((input, ModbusADU {
        transaction_id,
        protocol_id,
        length,
        unit_id,
        pdu,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        std::process::exit(1);
    }

    let data = fs::read(&args[1]).expect("Failed to read file");
    match parse_modbus_adu(&data) {
        Ok((remaining, adu)) => {
            println!("Parsed ADU: {:?}", adu);
            if !remaining.is_empty() {
                println!("Remaining unparsed data: {:?}", remaining);
            }
        },
        Err(e) => eprintln!("Failed to parse Modbus ADU: {:?}", e),
    }
}