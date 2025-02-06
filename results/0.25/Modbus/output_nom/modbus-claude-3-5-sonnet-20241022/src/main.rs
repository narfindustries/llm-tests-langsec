use nom::{
    bytes::complete::take,
    combinator::map,
    number::complete::{be_u16, be_u8},
    sequence::tuple,
    IResult,
};
use std::{env, fs, io::Read};

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
    WriteMultipleCoils = 0x0F,
    WriteMultipleRegisters = 0x10,
    ReadFileRecord = 0x14,
    WriteFileRecord = 0x15,
    MaskWriteRegister = 0x16,
    ReadWriteMultipleRegisters = 0x17,
    ReadFifoQueue = 0x18,
    Error = 0x80,
}

#[derive(Debug)]
enum ModbusRequest {
    ReadRequest {
        start_address: u16,
        quantity: u16,
    },
    WriteSingleRequest {
        output_address: u16,
        output_value: u16,
    },
    WriteMultipleRequest {
        start_address: u16,
        quantity: u16,
        byte_count: u8,
        data: Vec<u8>,
    },
}

#[derive(Debug)]
enum ModbusResponse {
    ReadResponse {
        byte_count: u8,
        data: Vec<u8>,
    },
    WriteSingleResponse {
        output_address: u16,
        output_value: u16,
    },
    WriteMultipleResponse {
        start_address: u16,
        quantity: u16,
    },
    ErrorResponse {
        exception_code: u8,
    },
}

#[derive(Debug)]
struct ModbusPacket {
    header: MbapHeader,
    function: FunctionCode,
    data: Option<ModbusRequest>,
    response: Option<ModbusResponse>,
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
        0x0F => FunctionCode::WriteMultipleCoils,
        0x10 => FunctionCode::WriteMultipleRegisters,
        0x14 => FunctionCode::ReadFileRecord,
        0x15 => FunctionCode::WriteFileRecord,
        0x16 => FunctionCode::MaskWriteRegister,
        0x17 => FunctionCode::ReadWriteMultipleRegisters,
        0x18 => FunctionCode::ReadFifoQueue,
        _ => FunctionCode::Error,
    })(input)
}

fn parse_read_request(input: &[u8]) -> IResult<&[u8], ModbusRequest> {
    map(
        tuple((be_u16, be_u16)),
        |(start_address, quantity)| ModbusRequest::ReadRequest {
            start_address,
            quantity,
        },
    )(input)
}

fn parse_write_single_request(input: &[u8]) -> IResult<&[u8], ModbusRequest> {
    map(
        tuple((be_u16, be_u16)),
        |(output_address, output_value)| ModbusRequest::WriteSingleRequest {
            output_address,
            output_value,
        },
    )(input)
}

fn parse_write_multiple_request(input: &[u8]) -> IResult<&[u8], ModbusRequest> {
    let (input, (start_address, quantity, byte_count)) =
        tuple((be_u16, be_u16, be_u8))(input)?;
    let (input, data) = take(byte_count as usize)(input)?;
    Ok((
        input,
        ModbusRequest::WriteMultipleRequest {
            start_address,
            quantity,
            byte_count,
            data: data.to_vec(),
        },
    ))
}

fn parse_read_response(input: &[u8]) -> IResult<&[u8], ModbusResponse> {
    let (input, byte_count) = be_u8(input)?;
    let (input, data) = take(byte_count as usize)(input)?;
    Ok((
        input,
        ModbusResponse::ReadResponse {
            byte_count,
            data: data.to_vec(),
        },
    ))
}

fn parse_write_single_response(input: &[u8]) -> IResult<&[u8], ModbusResponse> {
    map(
        tuple((be_u16, be_u16)),
        |(output_address, output_value)| ModbusResponse::WriteSingleResponse {
            output_address,
            output_value,
        },
    )(input)
}

fn parse_write_multiple_response(input: &[u8]) -> IResult<&[u8], ModbusResponse> {
    map(
        tuple((be_u16, be_u16)),
        |(start_address, quantity)| ModbusResponse::WriteMultipleResponse {
            start_address,
            quantity,
        },
    )(input)
}

fn parse_error_response(input: &[u8]) -> IResult<&[u8], ModbusResponse> {
    map(be_u8, |exception_code| {
        ModbusResponse::ErrorResponse { exception_code }
    })(input)
}

fn parse_modbus_packet(input: &[u8]) -> IResult<&[u8], ModbusPacket> {
    let (input, header) = parse_mbap_header(input)?;
    let (input, function) = parse_function_code(input)?;

    let (input, data) = match function {
        FunctionCode::ReadCoils
        | FunctionCode::ReadDiscreteInputs
        | FunctionCode::ReadHoldingRegisters
        | FunctionCode::ReadInputRegisters => {
            let (input, request) = parse_read_request(input)?;
            (input, Some(request))
        }
        FunctionCode::WriteSingleCoil | FunctionCode::WriteSingleRegister => {
            let (input, request) = parse_write_single_request(input)?;
            (input, Some(request))
        }
        FunctionCode::WriteMultipleCoils | FunctionCode::WriteMultipleRegisters => {
            let (input, request) = parse_write_multiple_request(input)?;
            (input, Some(request))
        }
        _ => (input, None),
    };

    let (input, response) = match function {
        FunctionCode::Error => {
            let (input, response) = parse_error_response(input)?;
            (input, Some(response))
        }
        FunctionCode::ReadCoils
        | FunctionCode::ReadDiscreteInputs
        | FunctionCode::ReadHoldingRegisters
        | FunctionCode::ReadInputRegisters => {
            let (input, response) = parse_read_response(input)?;
            (input, Some(response))
        }
        FunctionCode::WriteSingleCoil | FunctionCode::WriteSingleRegister => {
            let (input, response) = parse_write_single_response(input)?;
            (input, Some(response))
        }
        FunctionCode::WriteMultipleCoils | FunctionCode::WriteMultipleRegisters => {
            let (input, response) = parse_write_multiple_response(input)?;
            (input, Some(response))
        }
        _ => (input, None),
    };

    Ok((
        input,
        ModbusPacket {
            header,
            function,
            data,
            response,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        return;
    }

    let mut file = fs::File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)
        .expect("Failed to read file contents");

    match parse_modbus_packet(&buffer) {
        Ok((remaining, packet)) => {
            println!("Parsed packet: {:?}", packet);
            println!("Remaining bytes: {:?}", remaining);
        }
        Err(e) => eprintln!("Failed to parse Modbus packet: {:?}", e),
    }
}