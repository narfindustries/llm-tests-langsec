use std::fs::File;
use std::io::Read;
use std::env;
use nom::{
    IResult,
    bytes::complete::take,
    number::complete::{be_u8, be_u16},
    combinator::map,
    sequence::tuple,
};

#[derive(Debug)]
struct ModbusRTU {
    address: u8,
    function_code: u8,
    data: Vec<u8>,
    crc: u16,
}

#[derive(Debug)]
struct ModbusTCP {
    transaction_id: u16,
    protocol_id: u16,
    length: u16,
    unit_id: u8,
    function_code: u8,
    data: Vec<u8>,
}

fn parse_modbus_rtu(input: &[u8]) -> IResult<&[u8], ModbusRTU> {
    let (input, (address, function_code, data, crc)) = tuple((
        be_u8,
        be_u8,
        map(take(input.len() - 4), |d: &[u8]| d.to_vec()), // take until CRC
        be_u16
    ))(input)?;

    Ok((input, ModbusRTU {
        address,
        function_code,
        data,
        crc,
    }))
}

fn parse_modbus_tcp(input: &[u8]) -> IResult<&[u8], ModbusTCP> {
    let (input, (transaction_id, protocol_id, length, unit_id, function_code, data)) = tuple((
        be_u16,
        be_u16,
        be_u16,
        be_u8,
        be_u8,
        map(take(input.len() - 7), |d: &[u8]| d.to_vec()), // take until end
    ))(input)?;

    Ok((input, ModbusTCP {
        transaction_id,
        protocol_id,
        length,
        unit_id,
        function_code,
        data,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Unable to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Unable to read file");

    match parse_modbus_rtu(&buffer) {
        Ok((_, rtu_frame)) => println!("Parsed RTU Frame: {:?}", rtu_frame),
        Err(_) => println!("Failed to parse as RTU Frame"),
    }

    match parse_modbus_tcp(&buffer) {
        Ok((_, tcp_frame)) => println!("Parsed TCP Frame: {:?}", tcp_frame),
        Err(_) => println!("Failed to parse as TCP Frame"),
    }
}