use nom::{
    IResult, 
    bytes::streaming::take,
    number::streaming::{be_u8, be_u16, be_u32}
};
use std::fs::File;
use std::io::{self, Read};
use std::env;
use std::process;

#[derive(Debug)]
enum ModbusFrame {
    RTUFrame {
        address: u8,
        function_code: u8,
        data: Vec<u8>,
        crc: u16,
    },
    TCPFrame {
        transaction_id: u16,
        protocol_id: u16,
        length: u16,
        unit_id: u8,
        function_code: u8,
        data: Vec<u8>,
    },
}

fn parse_modbus_rtu(input: &[u8]) -> IResult<&[u8], ModbusFrame> {
    let (input, address) = be_u8(input)?;
    let (input, function_code) = be_u8(input)?;
    let (input, data) = take(input.len() - 2)(input)?;
    let (input, crc) = be_u16(input)?;
    
    Ok((input, ModbusFrame::RTUFrame {
        address,
        function_code,
        data: data.to_vec(),
        crc,
    }))
}

fn parse_modbus_tcp(input: &[u8]) -> IResult<&[u8], ModbusFrame> {
    let (input, transaction_id) = be_u16(input)?;
    let (input, protocol_id) = be_u16(input)?;
    let (input, length) = be_u16(input)?;
    let (input, unit_id) = be_u8(input)?;
    let (input, function_code) = be_u8(input)?;
    let (input, data) = take(length as usize - 2)(input)?;

    Ok((input, ModbusFrame::TCPFrame {
        transaction_id,
        protocol_id,
        length,
        unit_id,
        function_code,
        data: data.to_vec(),
    }))
}

fn parse_modbus(input: &[u8]) -> IResult<&[u8], ModbusFrame> {
    if input.len() > 6 && input[2] == 0 && input[3] == 0 {
        parse_modbus_tcp(input)
    } else {
        parse_modbus_rtu(input)
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: modbus_parser <file_path>");
        process::exit(1);
    }

    let file_path = &args[1];
    let mut file = File::open(file_path).expect("Failed to open the file");
    let mut buffer = Vec::new();

    file.read_to_end(&mut buffer).expect("Failed to read the file");

    match parse_modbus(&buffer) {
        Ok((_rest, frame)) => {
            println!("{:?}", frame);
        },
        Err(err) => {
            eprintln!("Failed to parse Modbus data: {:?}", err);
        },
    }
}