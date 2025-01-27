use nom::{
    bytes::complete::{tag, take},
    combinator::{map, verify},
    error::ErrorKind,
    multi::length_data,
    number::complete::{be_u16, be_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
pub struct ModbusADU {
    transaction_id: u16,
    protocol_id: u16,
    length: u16,
    unit_id: u8,
    pdu: ModbusPDU,
}

#[derive(Debug)]
pub enum ModbusPDU {
    ReadCoils(ReadRequest),
    ReadDiscreteInputs(ReadRequest),
    ReadHoldingRegisters(ReadRequest),
    ReadInputRegisters(ReadRequest),
    WriteSingleCoil(WriteSingleRequest),
    WriteSingleRegister(WriteSingleRequest),
    WriteMultipleCoils(WriteMultipleRequest),
    WriteMultipleRegisters(WriteMultipleRequest),
    Response(ModbusResponse),
    Error(ModbusError),
}

#[derive(Debug)]
pub struct ReadRequest {
    starting_address: u16,
    quantity: u16,
}

#[derive(Debug)]
pub struct WriteSingleRequest {
    address: u16,
    value: u16,
}

#[derive(Debug)]
pub struct WriteMultipleRequest {
    starting_address: u16,
    quantity: u16,
    byte_count: u8,
    values: Vec<u8>,
}

#[derive(Debug)]
pub struct ModbusResponse {
    function_code: u8,
    byte_count: u8,
    data: Vec<u8>,
}

#[derive(Debug)]
pub struct ModbusError {
    function_code: u8,
    exception_code: u8,
}

fn parse_modbus_adu(input: &[u8]) -> IResult<&[u8], ModbusADU> {
    let (input, (transaction_id, protocol_id, length, unit_id)) =
        tuple((be_u16, be_u16, be_u16, be_u8))(input)?;

    let (input, pdu) = parse_modbus_pdu(input)?;

    Ok((
        input,
        ModbusADU {
            transaction_id,
            protocol_id,
            length,
            unit_id,
            pdu,
        },
    ))
}

fn parse_modbus_pdu(input: &[u8]) -> IResult<&[u8], ModbusPDU> {
    let (input, function_code) = be_u8(input)?;

    match function_code {
        0x01 => map(parse_read_request, ModbusPDU::ReadCoils)(input),
        0x02 => map(parse_read_request, ModbusPDU::ReadDiscreteInputs)(input),
        0x03 => map(parse_read_request, ModbusPDU::ReadHoldingRegisters)(input),
        0x04 => map(parse_read_request, ModbusPDU::ReadInputRegisters)(input),
        0x05 => map(parse_write_single_request, ModbusPDU::WriteSingleCoil)(input),
        0x06 => map(parse_write_single_request, ModbusPDU::WriteSingleRegister)(input),
        0x0F => map(parse_write_multiple_request, ModbusPDU::WriteMultipleCoils)(input),
        0x10 => map(parse_write_multiple_request, ModbusPDU::WriteMultipleRegisters)(input),
        fc if fc & 0x80 != 0 => map(
            |input| parse_error(input, fc),
            ModbusPDU::Error,
        )(input),
        _ => map(
            |input| parse_response(input, function_code),
            ModbusPDU::Response,
        )(input),
    }
}

fn parse_read_request(input: &[u8]) -> IResult<&[u8], ReadRequest> {
    let (input, (starting_address, quantity)) = tuple((be_u16, be_u16))(input)?;

    Ok((
        input,
        ReadRequest {
            starting_address,
            quantity,
        },
    ))
}

fn parse_write_single_request(input: &[u8]) -> IResult<&[u8], WriteSingleRequest> {
    let (input, (address, value)) = tuple((be_u16, be_u16))(input)?;

    Ok((
        input,
        WriteSingleRequest { address, value },
    ))
}

fn parse_write_multiple_request(input: &[u8]) -> IResult<&[u8], WriteMultipleRequest> {
    let (input, (starting_address, quantity, byte_count)) =
        tuple((be_u16, be_u16, be_u8))(input)?;
    let (input, values) = take(byte_count as usize)(input)?;

    Ok((
        input,
        WriteMultipleRequest {
            starting_address,
            quantity,
            byte_count,
            values: values.to_vec(),
        },
    ))
}

fn parse_response(input: &[u8], function_code: u8) -> IResult<&[u8], ModbusResponse> {
    let (input, byte_count) = be_u8(input)?;
    let (input, data) = take(byte_count as usize)(input)?;

    Ok((
        input,
        ModbusResponse {
            function_code,
            byte_count,
            data: data.to_vec(),
        },
    ))
}

fn parse_error(input: &[u8], function_code: u8) -> IResult<&[u8], ModbusError> {
    let (input, exception_code) = be_u8(input)?;

    Ok((
        input,
        ModbusError {
            function_code,
            exception_code,
        },
    ))
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

    match parse_modbus_adu(&buffer) {
        Ok((remaining, adu)) => {
            println!("Parsed ADU: {:?}", adu);
            if !remaining.is_empty() {
                println!("Warning: {} bytes remaining", remaining.len());
            }
        }
        Err(e) => eprintln!("Error parsing Modbus ADU: {:?}", e),
    }

    Ok(())
}