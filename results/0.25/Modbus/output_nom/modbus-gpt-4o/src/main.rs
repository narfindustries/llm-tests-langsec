use nom::{
    bytes::complete::take,
    combinator::map_res,
    number::complete::{be_u16, be_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

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
    Unknown(u8),
}

impl From<u8> for ModbusFunctionCode {
    fn from(code: u8) -> Self {
        match code {
            0x01 => ModbusFunctionCode::ReadCoils,
            0x02 => ModbusFunctionCode::ReadDiscreteInputs,
            0x03 => ModbusFunctionCode::ReadHoldingRegisters,
            0x04 => ModbusFunctionCode::ReadInputRegisters,
            0x05 => ModbusFunctionCode::WriteSingleCoil,
            0x06 => ModbusFunctionCode::WriteSingleRegister,
            0x0F => ModbusFunctionCode::WriteMultipleCoils,
            0x10 => ModbusFunctionCode::WriteMultipleRegisters,
            _ => ModbusFunctionCode::Unknown(code),
        }
    }
}

#[derive(Debug)]
struct ModbusHeader {
    transaction_id: u16,
    protocol_id: u16,
    length: u16,
    unit_id: u8,
}

#[derive(Debug)]
struct ModbusPDU {
    function_code: ModbusFunctionCode,
    data: Vec<u8>,
}

#[derive(Debug)]
struct ModbusFrame {
    header: ModbusHeader,
    pdu: ModbusPDU,
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

fn parse_modbus_pdu(input: &[u8]) -> IResult<&[u8], ModbusPDU> {
    let (input, function_code) = map_res(be_u8, |code| Ok(ModbusFunctionCode::from(code)))(input)?;
    let (input, data) = take(input.len())(input)?;
    Ok((
        input,
        ModbusPDU {
            function_code,
            data: data.to_vec(),
        },
    ))
}

fn parse_modbus_frame(input: &[u8]) -> IResult<&[u8], ModbusFrame> {
    let (input, header) = parse_modbus_header(input)?;
    let (input, pdu) = parse_modbus_pdu(input)?;
    Ok((input, ModbusFrame { header, pdu }))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary file>", args[0]);
        return Ok(());
    }

    let filename = &args[1];
    let mut file = File::open(filename)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_modbus_frame(&buffer) {
        Ok((_, frame)) => println!("{:?}", frame),
        Err(e) => eprintln!("Failed to parse Modbus frame: {:?}", e),
    }

    Ok(())
}