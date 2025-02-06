use std::fs::File;
use std::io::Read;
use std::env;
use nom::{
    IResult,
    bytes::complete::take,
    number::complete::{be_u8, be_u16},
};

#[derive(Debug)]
enum ModbusFrame {
    RTU(ModbusRTUFrame),
    TCP(ModbusTCPFrame),
}

#[derive(Debug)]
struct ModbusRTUFrame {
    address: u8,
    function_code: u8,
    data: Vec<u8>,
    crc: u16,
}

#[derive(Debug)]
struct ModbusTCPFrame {
    transaction_id: u16,
    protocol_id: u16,
    length: u16,
    unit_id: u8,
    function_code: u8,
    data: Vec<u8>,
}

fn parse_modbus_rtu(input: &[u8]) -> IResult<&[u8], ModbusRTUFrame> {
    let (input, address) = be_u8(input)?;
    let (input, function_code) = be_u8(input)?;
    let data_length = input.len() - 2; // excluding CRC
    let (input, data) = take(data_length)(input)?;
    let (input, crc) = be_u16(input)?;
    Ok((input, ModbusRTUFrame {
        address,
        function_code,
        data: data.to_vec(),
        crc,
    }))
}

fn parse_modbus_tcp(input: &[u8]) -> IResult<&[u8], ModbusTCPFrame> {
    let (input, transaction_id) = be_u16(input)?;
    let (input, protocol_id) = be_u16(input)?;
    let (input, length) = be_u16(input)?;
    let (input, unit_id) = be_u8(input)?;
    let (input, function_code) = be_u8(input)?;
    let data_length = length as usize - 2; // excluding unit_id and function_code
    let (input, data) = take(data_length)(input)?;
    Ok((input, ModbusTCPFrame {
        transaction_id,
        protocol_id,
        length,
        unit_id,
        function_code,
        data: data.to_vec(),
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Unable to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Unable to read file");

    match parse_modbus_rtu(&buffer) {
        Ok((_, rtu_frame)) => {
            println!("Parsed Modbus RTU Frame: {:?}", rtu_frame);
        }
        Err(_) => {
            match parse_modbus_tcp(&buffer) {
                Ok((_, tcp_frame)) => {
                    println!("Parsed Modbus TCP Frame: {:?}", tcp_frame);
                }
                Err(_) => {
                    eprintln!("Failed to parse the input as either Modbus RTU or TCP frame.");
                }
            }
        }
    }
}