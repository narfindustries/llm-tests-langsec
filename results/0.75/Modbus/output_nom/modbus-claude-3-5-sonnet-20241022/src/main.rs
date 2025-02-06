use nom::{
    bytes::complete::take,
    number::complete::{be_u16, be_u8},
    IResult,
    combinator::map,
    sequence::tuple,
};
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug)]
struct MbapHeader {
    transaction_id: u16,
    protocol_id: u16,
    length: u16,
    unit_id: u8,
}

#[derive(Debug)]
#[repr(u8)]
enum FunctionCode {
    ReadCoils = 0x01,
    ReadDiscreteInputs = 0x02,
    ReadHoldingRegisters = 0x03,
    ReadInputRegisters = 0x04,
    WriteSingleCoil = 0x05,
    WriteSingleRegister = 0x06,
    ReadExceptionStatus = 0x07,
    Diagnostics = 0x08,
    GetCommEventCounter = 0x0B,
    GetCommEventLog = 0x0C,
    WriteMultipleCoils = 0x0F,
    WriteMultipleRegisters = 0x10,
    ReportServerId = 0x11,
    ReadFileRecord = 0x14,
    WriteFileRecord = 0x15,
    MaskWriteRegister = 0x16,
    ReadWriteMultipleRegisters = 0x17,
    ReadFifoQueue = 0x18,
    EncapsulatedInterfaceTransport = 0x2B,
    Error(u8),
}

#[derive(Debug)]
enum ModbusRequest {
    ReadCoils {
        starting_address: u16,
        quantity: u16,
    },
    WriteSingleCoil {
        address: u16,
        value: u16,
    },
    WriteMultipleCoils {
        starting_address: u16,
        quantity: u16,
        byte_count: u8,
        values: Vec<u8>,
    },
    ReadHoldingRegisters {
        starting_address: u16,
        quantity: u16,
    },
    WriteSingleRegister {
        address: u16,
        value: u16,
    },
    WriteMultipleRegisters {
        starting_address: u16,
        quantity: u16,
        byte_count: u8,
        values: Vec<u16>,
    },
    Diagnostics {
        sub_function: u16,
        data: Vec<u8>,
    },
    EncapsulatedInterface {
        mei_type: u8,
        data: Vec<u8>,
    },
}

#[derive(Debug)]
struct ModbusPacket {
    header: MbapHeader,
    function: FunctionCode,
    data: ModbusRequest,
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

fn parse_function_code(input: &[u8]) -> IResult<&[u8], FunctionCode> {
    map(be_u8, |code| match code {
        0x01 => FunctionCode::ReadCoils,
        0x02 => FunctionCode::ReadDiscreteInputs,
        0x03 => FunctionCode::ReadHoldingRegisters,
        0x04 => FunctionCode::ReadInputRegisters,
        0x05 => FunctionCode::WriteSingleCoil,
        0x06 => FunctionCode::WriteSingleRegister,
        0x07 => FunctionCode::ReadExceptionStatus,
        0x08 => FunctionCode::Diagnostics,
        0x0B => FunctionCode::GetCommEventCounter,
        0x0C => FunctionCode::GetCommEventLog,
        0x0F => FunctionCode::WriteMultipleCoils,
        0x10 => FunctionCode::WriteMultipleRegisters,
        0x11 => FunctionCode::ReportServerId,
        0x14 => FunctionCode::ReadFileRecord,
        0x15 => FunctionCode::WriteFileRecord,
        0x16 => FunctionCode::MaskWriteRegister,
        0x17 => FunctionCode::ReadWriteMultipleRegisters,
        0x18 => FunctionCode::ReadFifoQueue,
        0x2B => FunctionCode::EncapsulatedInterfaceTransport,
        code if code >= 0x80 => FunctionCode::Error(code - 0x80),
        _ => FunctionCode::Error(0),
    })(input)
}

fn parse_read_request(input: &[u8]) -> IResult<&[u8], ModbusRequest> {
    map(
        tuple((be_u16, be_u16)),
        |(starting_address, quantity)| ModbusRequest::ReadCoils {
            starting_address,
            quantity,
        },
    )(input)
}

fn parse_write_single_request(input: &[u8]) -> IResult<&[u8], ModbusRequest> {
    map(
        tuple((be_u16, be_u16)),
        |(address, value)| ModbusRequest::WriteSingleCoil {
            address,
            value,
        },
    )(input)
}

fn parse_write_multiple_coils(input: &[u8]) -> IResult<&[u8], ModbusRequest> {
    let (input, (starting_address, quantity, byte_count)) = 
        tuple((be_u16, be_u16, be_u8))(input)?;
    let (input, values) = take(byte_count as usize)(input)?;
    Ok((input, ModbusRequest::WriteMultipleCoils {
        starting_address,
        quantity,
        byte_count,
        values: values.to_vec(),
    }))
}

fn parse_write_multiple_registers(input: &[u8]) -> IResult<&[u8], ModbusRequest> {
    let (input, (starting_address, quantity, byte_count)) = 
        tuple((be_u16, be_u16, be_u8))(input)?;
    let mut values = Vec::new();
    let mut remaining = input;
    for _ in 0..(byte_count as usize / 2) {
        let (new_input, value) = be_u16(remaining)?;
        values.push(value);
        remaining = new_input;
    }
    Ok((remaining, ModbusRequest::WriteMultipleRegisters {
        starting_address,
        quantity,
        byte_count,
        values,
    }))
}

fn parse_diagnostics(input: &[u8]) -> IResult<&[u8], ModbusRequest> {
    let (input, sub_function) = be_u16(input)?;
    let (input, data) = take(2usize)(input)?;
    Ok((input, ModbusRequest::Diagnostics {
        sub_function,
        data: data.to_vec(),
    }))
}

fn parse_encapsulated_interface(input: &[u8]) -> IResult<&[u8], ModbusRequest> {
    let (input, mei_type) = be_u8(input)?;
    let (input, data) = take(input.len())(input)?;
    Ok((input, ModbusRequest::EncapsulatedInterface {
        mei_type,
        data: data.to_vec(),
    }))
}

fn parse_modbus_request<'a>(function: &FunctionCode, input: &'a [u8]) -> IResult<&'a [u8], ModbusRequest> {
    match function {
        FunctionCode::ReadCoils | 
        FunctionCode::ReadDiscreteInputs |
        FunctionCode::ReadHoldingRegisters |
        FunctionCode::ReadInputRegisters => parse_read_request(input),
        FunctionCode::WriteSingleCoil |
        FunctionCode::WriteSingleRegister => parse_write_single_request(input),
        FunctionCode::WriteMultipleCoils => parse_write_multiple_coils(input),
        FunctionCode::WriteMultipleRegisters => parse_write_multiple_registers(input),
        FunctionCode::Diagnostics => parse_diagnostics(input),
        FunctionCode::EncapsulatedInterfaceTransport => parse_encapsulated_interface(input),
        _ => Ok((input, ModbusRequest::ReadCoils { starting_address: 0, quantity: 0 })),
    }
}

fn parse_modbus_packet(input: &[u8]) -> IResult<&[u8], ModbusPacket> {
    let (input, header) = parse_mbap_header(input)?;
    let (input, function) = parse_function_code(input)?;
    let (input, data) = parse_modbus_request(&function, input)?;
    Ok((input, ModbusPacket {
        header,
        function,
        data,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_modbus_packet(&buffer) {
        Ok((remaining, packet)) => {
            println!("Parsed packet: {:?}", packet);
            println!("Remaining bytes: {:?}", remaining);
        }
        Err(e) => {
            eprintln!("Failed to parse Modbus packet: {:?}", e);
        }
    }
}