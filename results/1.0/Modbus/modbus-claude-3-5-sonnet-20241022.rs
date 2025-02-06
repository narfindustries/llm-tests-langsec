use nom::{
    bytes::complete::take,
    combinator::map,
    error::ErrorKind,
    number::complete::{be_u16, be_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct ModbusTCP {
    transaction_id: u16,
    protocol_id: u16,
    length: u16,
    unit_id: u8,
    pdu: ModbusPDU,
}

#[derive(Debug)]
enum ModbusPDU {
    ReadCoilsRequest {
        starting_address: u16,
        quantity: u16,
    },
    ReadCoilsResponse {
        byte_count: u8,
        coil_status: Vec<u8>,
    },
    ReadDiscreteInputsRequest {
        starting_address: u16,
        quantity: u16,
    },
    ReadDiscreteInputsResponse {
        byte_count: u8,
        input_status: Vec<u8>,
    },
    ReadHoldingRegistersRequest {
        starting_address: u16,
        quantity: u16,
    },
    ReadHoldingRegistersResponse {
        byte_count: u8,
        register_values: Vec<u16>,
    },
    ReadInputRegistersRequest {
        starting_address: u16,
        quantity: u16,
    },
    ReadInputRegistersResponse {
        byte_count: u8,
        register_values: Vec<u16>,
    },
    WriteSingleCoilRequest {
        output_address: u16,
        output_value: u16,
    },
    WriteSingleCoilResponse {
        output_address: u16,
        output_value: u16,
    },
    WriteSingleRegisterRequest {
        register_address: u16,
        register_value: u16,
    },
    WriteSingleRegisterResponse {
        register_address: u16,
        register_value: u16,
    },
    WriteMultipleCoilsRequest {
        starting_address: u16,
        quantity: u16,
        byte_count: u8,
        outputs_value: Vec<u8>,
    },
    WriteMultipleCoilsResponse {
        starting_address: u16,
        quantity: u16,
    },
    WriteMultipleRegistersRequest {
        starting_address: u16,
        quantity: u16,
        byte_count: u8,
        registers_value: Vec<u16>,
    },
    WriteMultipleRegistersResponse {
        starting_address: u16,
        quantity: u16,
    },
    ReadWriteMultipleRegistersRequest {
        read_starting_address: u16,
        quantity_to_read: u16,
        write_starting_address: u16,
        quantity_to_write: u16,
        write_byte_count: u8,
        write_registers_value: Vec<u16>,
    },
    ReadWriteMultipleRegistersResponse {
        byte_count: u8,
        read_registers_value: Vec<u16>,
    },
    MaskWriteRegisterRequest {
        reference_address: u16,
        and_mask: u16,
        or_mask: u16,
    },
    MaskWriteRegisterResponse {
        reference_address: u16,
        and_mask: u16,
        or_mask: u16,
    },
    ReadFIFOQueueRequest {
        fifo_pointer_address: u16,
    },
    ReadFIFOQueueResponse {
        byte_count: u16,
        fifo_count: u16,
        fifo_value_register: Vec<u16>,
    },
    Exception {
        function_code: u8,
        exception_code: u8,
    },
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_modbus_tcp(&buffer) {
        Ok((remaining, packet)) => {
            println!("Parsed packet: {:?}", packet);
            println!("Remaining bytes: {:?}", remaining);
        }
        Err(e) => println!("Error: {:?}", e),
    }

    Ok(())
}

fn parse_modbus_tcp(input: &[u8]) -> IResult<&[u8], ModbusTCP> {
    let (input, (transaction_id, protocol_id, length, unit_id)) =
        tuple((be_u16, be_u16, be_u16, be_u8))(input)?;

    let (input, pdu) = parse_pdu(input)?;

    Ok((
        input,
        ModbusTCP {
            transaction_id,
            protocol_id,
            length,
            unit_id,
            pdu,
        },
    ))
}

fn parse_pdu(input: &[u8]) -> IResult<&[u8], ModbusPDU> {
    let (input, function_code) = be_u8(input)?;

    match function_code {
        0x01 => parse_read_coils(input, function_code),
        0x02 => parse_read_discrete_inputs(input, function_code),
        0x03 => parse_read_holding_registers(input, function_code),
        0x04 => parse_read_input_registers(input, function_code),
        0x05 => parse_write_single_coil(input, function_code),
        0x06 => parse_write_single_register(input, function_code),
        0x0F => parse_write_multiple_coils(input, function_code),
        0x10 => parse_write_multiple_registers(input, function_code),
        0x16 => parse_mask_write_register(input, function_code),
        0x17 => parse_read_write_multiple_registers(input, function_code),
        0x18 => parse_read_fifo_queue(input, function_code),
        fc if fc >= 0x80 => parse_exception(input, function_code),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        ))),
    }
}

fn parse_exception(input: &[u8], function_code: u8) -> IResult<&[u8], ModbusPDU> {
    let (input, exception_code) = be_u8(input)?;
    Ok((input, ModbusPDU::Exception {
        function_code,
        exception_code,
    }))
}

fn parse_read_coils(input: &[u8], function_code: u8) -> IResult<&[u8], ModbusPDU> {
    if function_code == 0x01 {
        let (input, (starting_address, quantity)) = tuple((be_u16, be_u16))(input)?;
        Ok((
            input,
            ModbusPDU::ReadCoilsRequest {
                starting_address,
                quantity,
            },
        ))
    } else {
        let (input, byte_count) = be_u8(input)?;
        let (input, coil_status) = take(byte_count as usize)(input)?;
        Ok((
            input,
            ModbusPDU::ReadCoilsResponse {
                byte_count,
                coil_status: coil_status.to_vec(),
            },
        ))
    }
}

fn parse_read_discrete_inputs(input: &[u8], function_code: u8) -> IResult<&[u8], ModbusPDU> {
    if function_code == 0x02 {
        let (input, (starting_address, quantity)) = tuple((be_u16, be_u16))(input)?;
        Ok((
            input,
            ModbusPDU::ReadDiscreteInputsRequest {
                starting_address,
                quantity,
            },
        ))
    } else {
        let (input, byte_count) = be_u8(input)?;
        let (input, input_status) = take(byte_count as usize)(input)?;
        Ok((
            input,
            ModbusPDU::ReadDiscreteInputsResponse {
                byte_count,
                input_status: input_status.to_vec(),
            },
        ))
    }
}

fn parse_read_holding_registers(input: &[u8], function_code: u8) -> IResult<&[u8], ModbusPDU> {
    if function_code == 0x03 {
        let (input, (starting_address, quantity)) = tuple((be_u16, be_u16))(input)?;
        Ok((
            input,
            ModbusPDU::ReadHoldingRegistersRequest {
                starting_address,
                quantity,
            },
        ))
    } else {
        let (input, byte_count) = be_u8(input)?;
        let mut register_values = Vec::new();
        let mut remaining = input;
        for _ in 0..(byte_count / 2) {
            let (new_input, value) = be_u16(remaining)?;
            register_values.push(value);
            remaining = new_input;
        }
        Ok((
            remaining,
            ModbusPDU::ReadHoldingRegistersResponse {
                byte_count,
                register_values,
            },
        ))
    }
}

fn parse_read_input_registers(input: &[u8], function_code: u8) -> IResult<&[u8], ModbusPDU> {
    if function_code == 0x04 {
        let (input, (starting_address, quantity)) = tuple((be_u16, be_u16))(input)?;
        Ok((
            input,
            ModbusPDU::ReadInputRegistersRequest {
                starting_address,
                quantity,
            },
        ))
    } else {
        let (input, byte_count) = be_u8(input)?;
        let mut register_values = Vec::new();
        let mut remaining = input;
        for _ in 0..(byte_count / 2) {
            let (new_input, value) = be_u16(remaining)?;
            register_values.push(value);
            remaining = new_input;
        }
        Ok((
            remaining,
            ModbusPDU::ReadInputRegistersResponse {
                byte_count,
                register_values,
            },
        ))
    }
}

fn parse_write_single_coil(input: &[u8], function_code: u8) -> IResult<&[u8], ModbusPDU> {
    let (input, (output_address, output_value)) = tuple((be_u16, be_u16))(input)?;
    if function_code == 0x05 {
        Ok((
            input,
            ModbusPDU::WriteSingleCoilRequest {
                output_address,
                output_value,
            },
        ))
    } else {
        Ok((
            input,
            ModbusPDU::WriteSingleCoilResponse {
                output_address,
                output_value,
            },
        ))
    }
}

fn parse_write_single_register(input: &[u8], function_code: u8) -> IResult<&[u8], ModbusPDU> {
    let (input, (register_address, register_value)) = tuple((be_u16, be_u16))(input)?;
    if function_code == 0x06 {
        Ok((
            input,
            ModbusPDU::WriteSingleRegisterRequest {
                register_address,
                register_value,
            },
        ))
    } else {
        Ok((
            input,
            ModbusPDU::WriteSingleRegisterResponse {
                register_address,
                register_value,
            },
        ))
    }
}

fn parse_write_multiple_coils(input: &[u8], function_code: u8) -> IResult<&[u8], ModbusPDU> {
    if function_code == 0x0F {
        let (input, (starting_address, quantity, byte_count)) =
            tuple((be_u16, be_u16, be_u8))(input)?;
        let (input, outputs_value) = take(byte_count as usize)(input)?;
        Ok((
            input,
            ModbusPDU::WriteMultipleCoilsRequest {
                starting_address,
                quantity,
                byte_count,
                outputs_value: outputs_value.to_vec(),
            },
        ))
    } else {
        let (input, (starting_address, quantity)) = tuple((be_u16, be_u16))(input)?;
        Ok((
            input,
            ModbusPDU::WriteMultipleCoilsResponse {
                starting_address,
                quantity,
            },
        ))
    }
}

fn parse_write_multiple_registers(input: &[u8], function_code: u8) -> IResult<&[u8], ModbusPDU> {
    if function_code == 0x10 {
        let (input, (starting_address, quantity, byte_count)) =
            tuple((be_u16, be_u16, be_u8))(input)?;
        let mut registers_value = Vec::new();
        let mut remaining = input;
        for _ in 0..(byte_count / 2) {
            let (new_input, value) = be_u16(remaining)?;
            registers_value.push(value);
            remaining = new_input;
        }
        Ok((
            remaining,
            ModbusPDU::WriteMultipleRegistersRequest {
                starting_address,
                quantity,
                byte_count,
                registers_value,
            },
        ))
    } else {
        let (input, (starting_address, quantity)) = tuple((be_u16, be_u16))(input)?;
        Ok((
            input,
            ModbusPDU::WriteMultipleRegistersResponse {
                starting_address,
                quantity,
            },
        ))
    }
}

fn parse_read_write_multiple_registers(input: &[u8], function_code: u8) -> IResult<&[u8], ModbusPDU> {
    if function_code == 0x17 {
        let (
            input,
            (
                read_starting_address,
                quantity_to_read,
                write_starting_address,
                quantity_to_write,
                write_byte_count,
            ),
        ) = tuple((be_u16, be_u16, be_u16, be_u16, be_u8))(input)?;
        let mut write_registers_value = Vec::new();
        let mut remaining = input;
        for _ in 0..(write_byte_count / 2) {
            let (new_input, value) = be_u16(remaining)?;
            write_registers_value.push(value);
            remaining = new_input;
        }
        Ok((
            remaining,
            ModbusPDU::ReadWriteMultipleRegistersRequest {
                read_starting_address,
                quantity_to_read,
                write_starting_address,
                quantity_to_write,
                write_byte_count,
                write_registers_value,
            },
        ))
    } else {
        let (input, byte_count) = be_u8(input)?;
        let mut read_registers_value = Vec::new();
        let mut remaining = input;
        for _ in 0..(byte_count / 2) {
            let (new_input, value) = be_u16(remaining)?;
            read_registers_value.push(value);
            remaining = new_input;
        }
        Ok((
            remaining,
            ModbusPDU::ReadWriteMultipleRegist