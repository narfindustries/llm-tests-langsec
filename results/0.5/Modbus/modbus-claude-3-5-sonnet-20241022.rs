use nom::{
    bytes::complete::{tag, take},
    combinator::{map, verify},
    multi::count,
    number::complete::{be_u16, be_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct MbapHeader {
    transaction_id: u16,
    protocol_id: u16,
    length: u16,
    unit_id: u8,
}

#[derive(Debug)]
enum ModbusFunction {
    ReadCoils(u16, u16),
    ReadDiscreteInputs(u16, u16),
    ReadHoldingRegisters(u16, u16),
    ReadInputRegisters(u16, u16),
    WriteSingleCoil(u16, u16),
    WriteSingleRegister(u16, u16),
    WriteMultipleCoils {
        start_address: u16,
        quantity: u16,
        byte_count: u8,
        values: Vec<u8>,
    },
    WriteMultipleRegisters {
        start_address: u16,
        quantity: u16,
        byte_count: u8,
        values: Vec<u16>,
    },
    ReadWriteMultipleRegisters {
        read_start: u16,
        read_quantity: u16,
        write_start: u16,
        write_quantity: u16,
        write_byte_count: u8,
        write_values: Vec<u16>,
    },
    Error {
        function: u8,
        exception: u8,
    },
}

#[derive(Debug)]
struct ModbusPacket {
    header: MbapHeader,
    function: ModbusFunction,
}

fn parse_mbap_header(input: &[u8]) -> IResult<&[u8], MbapHeader> {
    map(
        tuple((be_u16, be_u16, be_u16, be_u8)),
        |(transaction_id, protocol_id, length, unit_id)| MbapHeader {
            transaction_id,
            protocol_id,
            length,
            unit_id,
        },
    )(input)
}

fn parse_read_request(input: &[u8]) -> IResult<&[u8], (u16, u16)> {
    tuple((be_u16, be_u16))(input)
}

fn parse_write_single(input: &[u8]) -> IResult<&[u8], (u16, u16)> {
    tuple((be_u16, be_u16))(input)
}

fn parse_write_multiple_coils(input: &[u8]) -> IResult<&[u8], ModbusFunction> {
    let (input, (start_address, quantity, byte_count)) = tuple((be_u16, be_u16, be_u8))(input)?;
    let (input, values) = count(be_u8, byte_count as usize)(input)?;
    Ok((
        input,
        ModbusFunction::WriteMultipleCoils {
            start_address,
            quantity,
            byte_count,
            values,
        },
    ))
}

fn parse_write_multiple_registers(input: &[u8]) -> IResult<&[u8], ModbusFunction> {
    let (input, (start_address, quantity, byte_count)) = tuple((be_u16, be_u16, be_u8))(input)?;
    let (input, values) = count(be_u16, (byte_count as usize) / 2)(input)?;
    Ok((
        input,
        ModbusFunction::WriteMultipleRegisters {
            start_address,
            quantity,
            byte_count,
            values,
        },
    ))
}

fn parse_read_write_multiple_registers(input: &[u8]) -> IResult<&[u8], ModbusFunction> {
    let (input, (read_start, read_quantity, write_start, write_quantity, write_byte_count)) =
        tuple((be_u16, be_u16, be_u16, be_u16, be_u8))(input)?;
    let (input, write_values) = count(be_u16, (write_byte_count as usize) / 2)(input)?;
    Ok((
        input,
        ModbusFunction::ReadWriteMultipleRegisters {
            read_start,
            read_quantity,
            write_start,
            write_quantity,
            write_byte_count,
            write_values,
        },
    ))
}

fn parse_function(input: &[u8]) -> IResult<&[u8], ModbusFunction> {
    let (input, function_code) = be_u8(input)?;
    match function_code {
        0x01 => map(parse_read_request, |(start, quantity)| {
            ModbusFunction::ReadCoils(start, quantity)
        })(input),
        0x02 => map(parse_read_request, |(start, quantity)| {
            ModbusFunction::ReadDiscreteInputs(start, quantity)
        })(input),
        0x03 => map(parse_read_request, |(start, quantity)| {
            ModbusFunction::ReadHoldingRegisters(start, quantity)
        })(input),
        0x04 => map(parse_read_request, |(start, quantity)| {
            ModbusFunction::ReadInputRegisters(start, quantity)
        })(input),
        0x05 => map(parse_write_single, |(addr, value)| {
            ModbusFunction::WriteSingleCoil(addr, value)
        })(input),
        0x06 => map(parse_write_single, |(addr, value)| {
            ModbusFunction::WriteSingleRegister(addr, value)
        })(input),
        0x0F => parse_write_multiple_coils(input),
        0x10 => parse_write_multiple_registers(input),
        0x17 => parse_read_write_multiple_registers(input),
        code if code >= 0x80 => {
            let (input, exception) = be_u8(input)?;
            Ok((
                input,
                ModbusFunction::Error {
                    function: code & 0x7F,
                    exception,
                },
            ))
        }
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

fn parse_modbus_packet(input: &[u8]) -> IResult<&[u8], ModbusPacket> {
    let (input, header) = parse_mbap_header(input)?;
    let (input, function) = parse_function(input)?;
    Ok((input, ModbusPacket { header, function }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_modbus_packet(&buffer) {
        Ok((remaining, packet)) => {
            println!("Parsed packet: {:?}", packet);
            if !remaining.is_empty() {
                println!("Warning: {} bytes remaining", remaining.len());
            }
        }
        Err(e) => eprintln!("Failed to parse: {:?}", e),
    }
}