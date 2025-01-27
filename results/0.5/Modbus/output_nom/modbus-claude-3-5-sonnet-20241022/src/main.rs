use nom::{
    bytes::complete::{tag, take},
    combinator::{map, verify},
    number::complete::{be_u16, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

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
    ReadCoils(u16, u16),                    // Start address, Quantity
    ReadDiscreteInputs(u16, u16),           // Start address, Quantity
    ReadHoldingRegisters(u16, u16),         // Start address, Quantity
    ReadInputRegisters(u16, u16),           // Start address, Quantity
    WriteSingleCoil(u16, bool),             // Address, Value
    WriteSingleRegister(u16, u16),          // Address, Value
    WriteMultipleCoils(u16, u16, Vec<u8>),  // Start address, Quantity, Values
    WriteMultipleRegisters(u16, u16, Vec<u16>), // Start address, Quantity, Values
    ReadExceptionStatus,
    Diagnostic(u16, Vec<u8>),               // Sub-function, Data
    GetCommEventCounter,
    GetCommEventLog,
    ReportSlaveId,
    ReadFileRecord(Vec<u8>),                // Sub-requests
    WriteFileRecord(Vec<u8>),               // Sub-requests
    MaskWriteRegister(u16, u16, u16),       // Reference Address, And_Mask, Or_Mask
    ReadWriteMultipleRegisters(u16, u16, u16, u16, Vec<u16>), // Read Start, Read Quantity, Write Start, Write Quantity, Write Values
    ReadFifoQueue(u16),                     // FIFO Pointer Address
    Error(u8, u8),                          // Function code, Exception code
}

fn parse_modbus_tcp(input: &[u8]) -> IResult<&[u8], ModbusADU> {
    let (input, transaction_id) = be_u16(input)?;
    let (input, protocol_id) = verify(be_u16, |&x| x == 0)(input)?;
    let (input, length) = be_u16(input)?;
    let (input, unit_id) = be_u8(input)?;
    let (input, pdu) = parse_pdu(input)?;
    
    Ok((input, ModbusADU {
        transaction_id,
        protocol_id,
        length,
        unit_id,
        pdu,
    }))
}

fn parse_pdu(input: &[u8]) -> IResult<&[u8], ModbusPDU> {
    let (input, function_code) = be_u8(input)?;
    
    match function_code {
        0x01 => {
            let (input, (start, quantity)) = tuple((be_u16, be_u16))(input)?;
            Ok((input, ModbusPDU::ReadCoils(start, quantity)))
        },
        0x02 => {
            let (input, (start, quantity)) = tuple((be_u16, be_u16))(input)?;
            Ok((input, ModbusPDU::ReadDiscreteInputs(start, quantity)))
        },
        0x03 => {
            let (input, (start, quantity)) = tuple((be_u16, be_u16))(input)?;
            Ok((input, ModbusPDU::ReadHoldingRegisters(start, quantity)))
        },
        0x04 => {
            let (input, (start, quantity)) = tuple((be_u16, be_u16))(input)?;
            Ok((input, ModbusPDU::ReadInputRegisters(start, quantity)))
        },
        0x05 => {
            let (input, (address, value)) = tuple((be_u16, be_u16))(input)?;
            Ok((input, ModbusPDU::WriteSingleCoil(address, value == 0xFF00)))
        },
        0x06 => {
            let (input, (address, value)) = tuple((be_u16, be_u16))(input)?;
            Ok((input, ModbusPDU::WriteSingleRegister(address, value)))
        },
        0x0F => {
            let (input, (start, quantity, byte_count)) = tuple((be_u16, be_u16, be_u8))(input)?;
            let (input, values) = take(byte_count as usize)(input)?;
            Ok((input, ModbusPDU::WriteMultipleCoils(start, quantity, values.to_vec())))
        },
        0x10 => {
            let (input, (start, quantity, byte_count)) = tuple((be_u16, be_u16, be_u8))(input)?;
            let mut values = Vec::new();
            let mut remaining = input;
            for _ in 0..(byte_count / 2) {
                let (new_input, value) = be_u16(remaining)?;
                values.push(value);
                remaining = new_input;
            }
            Ok((remaining, ModbusPDU::WriteMultipleRegisters(start, quantity, values)))
        },
        0x07 => Ok((input, ModbusPDU::ReadExceptionStatus)),
        0x08 => {
            let (input, (sub_function, data)) = tuple((be_u16, take(2_usize)))(input)?;
            Ok((input, ModbusPDU::Diagnostic(sub_function, data.to_vec())))
        },
        0x0B => Ok((input, ModbusPDU::GetCommEventCounter)),
        0x0C => Ok((input, ModbusPDU::GetCommEventLog)),
        0x11 => Ok((input, ModbusPDU::ReportSlaveId)),
        0x14 => {
            let (input, byte_count) = be_u8(input)?;
            let (input, data) = take(byte_count as usize)(input)?;
            Ok((input, ModbusPDU::ReadFileRecord(data.to_vec())))
        },
        0x15 => {
            let (input, byte_count) = be_u8(input)?;
            let (input, data) = take(byte_count as usize)(input)?;
            Ok((input, ModbusPDU::WriteFileRecord(data.to_vec())))
        },
        0x16 => {
            let (input, (reference, and_mask, or_mask)) = tuple((be_u16, be_u16, be_u16))(input)?;
            Ok((input, ModbusPDU::MaskWriteRegister(reference, and_mask, or_mask)))
        },
        0x17 => {
            let (input, (read_start, read_quantity, write_start, write_quantity, byte_count)) = 
                tuple((be_u16, be_u16, be_u16, be_u16, be_u8))(input)?;
            let mut values = Vec::new();
            let mut remaining = input;
            for _ in 0..(byte_count / 2) {
                let (new_input, value) = be_u16(remaining)?;
                values.push(value);
                remaining = new_input;
            }
            Ok((remaining, ModbusPDU::ReadWriteMultipleRegisters(
                read_start, read_quantity, write_start, write_quantity, values)))
        },
        0x18 => {
            let (input, pointer) = be_u16(input)?;
            Ok((input, ModbusPDU::ReadFifoQueue(pointer)))
        },
        code if code & 0x80 != 0 => {
            let (input, exception_code) = be_u8(input)?;
            Ok((input, ModbusPDU::Error(code & 0x7F, exception_code)))
        },
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
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

    match parse_modbus_tcp(&buffer) {
        Ok((remaining, adu)) => {
            println!("Parsed ADU: {:?}", adu);
            if !remaining.is_empty() {
                println!("Warning: {} bytes remaining", remaining.len());
            }
        }
        Err(e) => eprintln!("Parse error: {:?}", e),
    }

    Ok(())
}