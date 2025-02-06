use nom::{
    bytes::complete::{take, take_while_m_n},
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct ModbusTCPFrame {
    transaction_id: u16,
    protocol_id: u16,
    length: u16,
    unit_id: u8,
    function_code: u8,
    data: Vec<u8>,
}

fn parse_modbus_tcp_frame(input: &[u8]) -> IResult<&[u8], ModbusTCPFrame> {
    let (input, transaction_id) = be_u16(input)?;
    let (input, protocol_id) = be_u16(input)?;
    let (input, length) = be_u16(input)?;
    let (input, unit_id) = be_u8(input)?;
    let (input, function_code) = be_u8(input)?;
    let (input, data) = take(length as usize - 2)(input)?; // Subtract 2 for unit_id and function_code

    Ok((
        input,
        ModbusTCPFrame {
            transaction_id,
            protocol_id,
            length,
            unit_id,
            function_code,
            data: data.to_vec(),
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <file_path>", args[0]);
        return Ok(());
    }

    let file_path = &args[1];
    let mut file = File::open(file_path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_modbus_tcp_frame(&buffer) {
        Ok((_remaining, frame)) => {
            println!("{:?}", frame);
        }
        Err(error) => {
            println!("Failed to parse Modbus TCP frame: {:?}", error);
        }
    }

    Ok(())
}